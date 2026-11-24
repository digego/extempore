#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Measure how the xtlang compiler's time grows with the size of its input.

Each family is synthetic xtlang which grows along one dimension (arithmetic
statements, captured variables, nesting depth, overloads, ...). A sweep
compiles each family at several sizes, times the compiler's phases, and prints
a table per family with a growth exponent for each phase between the two
largest sizes: about 1 is linear, 2 quadratic, 3 cubic.

    extras/compiler-stress/stress.py sweep                   # every family
    extras/compiler-stress/stress.py sweep arith nest_let --sizes 200,400,800
    extras/compiler-stress/stress.py profile closure_vars 800 \\
        --wrap xtc:codegen:compile:make-closure
    extras/compiler-stress/stress.py gen arith 400           # print the code

The runtime (runtime/*.xtm) is loaded from disk when extempore starts, so a
sweep copies it first (--snapshot, on by default): editing the compiler
during a long sweep then doesn't change the compiler half-way through it.
"""

import argparse
import math
import re
import shutil
import subprocess
import sys
import tempfile
from collections.abc import Callable
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
PROFILE = Path(__file__).resolve().parent / "profile.xtm"

# A family maps a size to (setup, timed): xtlang loaded untimed, then the
# timed form, which must define stress_fn.
Family = Callable[[int], tuple[str, str]]


def arith(n: int) -> tuple[str, str]:
    """Float arithmetic statements: literals and overloaded operators."""
    body = "\n".join(
        f"      (set! {'xyz'[i % 3]} (+ (* {'xyz'[(i + 1) % 3]} {1.0 + i / 7:.4f}) {i % 11}.5))"
        for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let ((x a) (y 0.0) (z 1.0))
{body}
      (+ x y z))))""",
    )


def int_arith(n: int) -> tuple[str, str]:
    """Integer arithmetic mixing widths (i32 and i64)."""
    body = "\n".join(
        f"      (set! {'ij'[i % 2]} (+ (* {'ij'[(i + 1) % 2]} {i % 7 + 1}) (convert (* k {i % 5 + 2}) i64)))"
        for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:i64)
    (let ((i a) (j 0) (k:i32 3))
{body}
      (+ i j))))""",
    )


def locals_(n: int) -> tuple[str, str]:
    """One let with n bindings."""
    binds = "\n".join(f"          (v{i} (* a {i}.25))" for i in range(n))
    total = " ".join(f"v{i}" for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let (
{binds})
      (+ {total}))))""",
    )


def typed_locals(n: int) -> tuple[str, str]:
    """Locals annotated with mixed types, each converted and summed."""
    tys = ["i32", "i64", "float", "double"]
    binds = "\n".join(
        f"          (t{i}:{tys[i % 4]} (convert a {tys[i % 4]}))" for i in range(n)
    )
    uses = "\n".join(
        f"      (set! acc (+ acc (convert t{i} double)))" for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let (
{binds}
          (acc 0.0))
{uses}
      acc)))""",
    )


def closure_vars(n: int) -> tuple[str, str]:
    """A closure capturing n doubles, like an instrument's voice."""
    binds = "\n".join(f"          (s{i}:double {i}.0)" for i in range(n))
    sets = "\n".join(
        f"          (set! s{i} (+ s{i} (* x {i % 9}.5)))" for i in range(n)
    )
    total = " ".join(f"s{i}" for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda ()
    (let (
{binds})
      (lambda (x:double)
{sets}
        (+ {total})))))""",
    )


def closure_mixed(n: int) -> tuple[str, str]:
    """A closure capturing n variables of mixed scalar and pointer types."""
    tys = ["i32", "i64", "float", "double", "i8*", "double*"]
    inits = {
        "i32": "0",
        "i64": "0",
        "float": "0.0",
        "double": "0.0",
        "i8*": "null",
        "double*": "null",
    }
    binds = "\n".join(
        f"          (s{i}:{tys[i % 6]} {inits[tys[i % 6]]})" for i in range(n)
    )
    uses = "\n".join(
        f"        (set! acc (+ acc (convert s{i} double)))"
        for i in range(n)
        if tys[i % 6] in ("i32", "i64", "float", "double")
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda ()
    (let (
{binds}
          (acc 0.0))
      (lambda (x:double)
{uses}
        acc))))""",
    )


def lambdas(n: int) -> tuple[str, str]:
    """n inner lambdas in one let (each captures everything in scope)."""
    binds = "\n".join(
        f"          (f{i} (lambda (x:double) (* x {i}.5)))" for i in range(n)
    )
    calls = "\n".join(f"        (set! y (f{i} y))" for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let (
{binds}
          (y a))
{calls}
      y)))""",
    )


def nest_let(n: int) -> tuple[str, str]:
    """n nested single-binding lets."""
    opening = "".join(f"(let ((w{i} (+ a {i}.0)))\n" for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
{opening}w{n - 1}{")" * n}))""",
    )


def nest_if(n: int) -> tuple[str, str]:
    """ifs nested n deep through their consequents."""
    expr = "a"
    for i in range(n):
        expr = f"(if (> a {i}.0) (+ {expr} 1.0) {i}.5)"
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    {expr}))""",
    )


def cond_chain(n: int) -> tuple[str, str]:
    """A cond of n clauses (an if chain n deep through the alternatives)."""
    clauses = "\n".join(f"          ((= a {i}) (* b {i}.5))" for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:i64 b:double)
    (cond
{clauses}
          (else 0.0))))""",
    )


def expr_depth(n: int) -> tuple[str, str]:
    """One right-nested arithmetic expression n deep."""
    expr = "a"
    for i in range(n):
        expr = f"(+ {i % 3}.5 {expr})"
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    {expr}))""",
    )


def left_expr(n: int) -> tuple[str, str]:
    """One left-nested arithmetic expression n deep."""
    expr = "a"
    for i in range(n):
        expr = f"(+ {expr} {i % 3}.5)"
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    {expr}))""",
    )


def chain_fwd(n: int) -> tuple[str, str]:
    """Untyped locals in a chain, typed by its first link."""
    binds = "\n".join(
        f"          (v{i} (+ {'a' if i == 0 else f'v{i - 1}'} 1.0))" for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let* (
{binds})
      v{n - 1})))""",
    )


def chain_back(n: int) -> tuple[str, str]:
    """Untyped integer locals in a chain, typed only by its last use."""
    binds = "\n".join(
        f"          (v{i} (+ {'a' if i == 0 else f'v{i - 1}'} 1))" for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a)
    (let* (
{binds}
          (out:i32 v{n - 1}))
      out)))""",
    )


def global_calls(n: int) -> tuple[str, str]:
    """Calls to n distinct global closures."""
    setup = "\n".join(
        f"(bind-func stress_g{i} (lambda (x:double) (* x {i}.5)))" for i in range(n)
    )
    calls = "\n".join(f"      (set! x (stress_g{i} x))" for i in range(n))
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let ((x a))
{calls}
      x)))""",
    )


def poly_calls(n: int) -> tuple[str, str]:
    """n calls to one ad-hoc polymorphic name with four overloads."""
    setup = """(bind-func stress_p:[double,double]* (lambda (x) (* x 2.0)))
(bind-func stress_p:[float,float]* (lambda (x) (* x 2.0)))
(bind-func stress_p:[i64,i64]* (lambda (x) (* x 2)))
(bind-func stress_p:[i32,i32]* (lambda (x) (* x 2)))"""
    names = ["d", "f", "i", "j"]
    calls = "\n".join(
        f"      (set! {names[k % 4]} (stress_p {names[k % 4]}))" for k in range(n)
    )
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let ((d a) (f:float 1.0) (i:i64 1) (j:i32 1))
{calls}
      (+ d (convert f double) (convert i double) (convert j double)))))""",
    )


def overload_count(n: int) -> tuple[str, str]:
    """50 calls to a name with n overloads."""
    setup = "\n".join(f"(bind-type StressO{i} <i64,double>)" for i in range(n)) + "\n"
    setup += "\n".join(
        f"(bind-func stress_o:[double,StressO{i}*]* (lambda (x) (tref x 1)))"
        for i in range(n)
    )
    calls = "\n".join("      (set! acc (+ acc (stress_o t)))" for _ in range(50))
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (t:StressO{n - 1}*)
    (let ((acc 0.0))
{calls}
      acc)))""",
    )


def generic_calls(n: int) -> tuple[str, str]:
    """n calls to a generic function at three types."""
    setup = "(bind-func stress_gen:[!a,!a,!a]* (lambda (x y) (+ x y)))"
    names = ["d", "f", "i"]
    calls = "\n".join(
        f"      (set! {names[k % 3]} (stress_gen {names[k % 3]} {names[k % 3]}))"
        for k in range(n)
    )
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let ((d a) (f:float 1.0) (i:i64 1))
{calls}
      (+ d (convert f double) (convert i double)))))""",
    )


def tuple_fields(n: int) -> tuple[str, str]:
    """A tuple of n doubles, every field read and written."""
    setup = f"(bind-type StressT <{','.join(['double'] * n)}>)"
    sets = "\n".join(
        f"      (tset! t {i} (+ (tref t {(i + 1) % n}) {i}.5))" for i in range(n)
    )
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (t:StressT*)
{sets}
    (tref t 0)))""",
    )


def pointers(n: int) -> tuple[str, str]:
    """n pointer reads and writes."""
    body = "\n".join(
        f"      (pset! p {i} (+ (pref p {(i * 7) % n}) {i}.5))" for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (p:double*)
    (begin
{body}
      (pref p 0))))""",
    )


def loops(n: int) -> tuple[str, str]:
    """n sequential dotimes loops."""
    counters = " ".join(f"(k{i}:i64 0)" for i in range(n))
    body = "\n".join(
        f"      (dotimes (k{i} 4) (set! acc (+ acc (convert k{i} double))))"
        for i in range(n)
    )
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (let ((acc a) {counters})
{body}
      acc)))""",
    )


def strings(n: int) -> tuple[str, str]:
    """n string literals."""
    body = "\n".join(f'      (printf "line {i} %f\\n" a)' for i in range(n))
    return (
        "",
        f"""(bind-func stress_fn
  (lambda (a:double)
    (begin
{body}
      a)))""",
    )


def session_funcs(n: int) -> tuple[str, str]:
    """One small function, compiled after n others are defined."""
    setup = "\n".join(
        f"(bind-func stress_h{i} (lambda (x:double) (+ (stress_h{i - 1} x) {i}.0)))"
        if i
        else "(bind-func stress_h0 (lambda (x:double) (* x 2.0)))"
        for i in range(n)
    )
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (a:double)
    (+ (stress_h{n - 1} a) (stress_h0 a))))""",
    )


def session_types(n: int) -> tuple[str, str]:
    """One small function, compiled after n named types are defined."""
    setup = "\n".join(
        f"(bind-type StressU{i} <i64,double,StressU{i - 1}*>)"
        if i
        else "(bind-type StressU0 <i64,double>)"
        for i in range(n)
    )
    return (
        setup,
        f"""(bind-func stress_fn
  (lambda (t:StressU{n - 1}*)
    (+ (convert (tref t 0) double) (tref t 1) (tref (tref t 2) 1))))""",
    )


FAMILIES: dict[str, Family] = {
    "arith": arith,
    "int_arith": int_arith,
    "locals": locals_,
    "typed_locals": typed_locals,
    "closure_vars": closure_vars,
    "closure_mixed": closure_mixed,
    "lambdas": lambdas,
    "nest_let": nest_let,
    "nest_if": nest_if,
    "cond_chain": cond_chain,
    "expr_depth": expr_depth,
    "left_expr": left_expr,
    "chain_fwd": chain_fwd,
    "chain_back": chain_back,
    "global_calls": global_calls,
    "poly_calls": poly_calls,
    "overload_count": overload_count,
    "generic_calls": generic_calls,
    "tuple_fields": tuple_fields,
    "pointers": pointers,
    "loops": loops,
    "strings": strings,
    "session_funcs": session_funcs,
    "session_types": session_types,
}

DEFAULT_SIZES = [200, 400, 800, 1600]

# Sizes for families whose cost per unit is high, unless --sizes is given.
SMALL_SIZES = {"lambdas": [10, 20, 40, 80]}

# The phase timers profile.xtm always reports, with short column names.
PHASES = {
    "xtc:driver:run": "run",
    "xtc:driver:syntactic-phase": "syntax",
    "xtc:driver:annotation-phase": "annot",
    "xtc:driver:type-phase": "types",
    "xtc:typecheck:coercion-run": "coerce",
    "xtc:infer:check-function": "infer",
    "xtc:infer:collect": "collect",
    "xtc:solve:run": "solve",
    "xtc:desugar:type-normalize": "normal",
    "xtc:typecheck:closure:convert": "clconv",
    "xtc:driver:semantic-phase": "semantic",
    "xtc:codegen:compiler": "codegen",
    "llvm:compile-ir": "llvm-ir",
}

Result = dict[str, float] | str  # seconds per column, or what went wrong


def snapshot(into: Path) -> Path:
    """A share dir with a frozen copy of runtime/ and the rest linked."""
    share = into / "share"
    share.mkdir()
    shutil.copytree(REPO / "runtime", share / "runtime")
    for name in ("libs", "examples", "assets"):
        if (REPO / name).exists():
            (share / name).symlink_to(REPO / name)
    return share


def run_case(
    workdir: Path,
    family: str,
    n: int,
    share: Path | None,
    wrap: list[str],
    timeout: int,
) -> tuple[str, Result]:
    """Compile one family at one size under profile.xtm."""
    setup, timed = FAMILIES[family](n)
    pre = workdir / f"{family}-{n}.pre.xtm"
    src = workdir / f"{family}-{n}.xtm"
    pre.write_text(setup + "\n")
    src.write_text(timed + "\n")
    cmd = [str(REPO / "build/extempore")]
    if share:
        cmd.append(f"--sharedir={share}")
    cmd += [f"--pre={pre}", f"--timed={src}", f"--label={family}-{n}"]
    if wrap:
        cmd.append(f"--wrap={','.join(wrap)}")
    cmd += ["--batch", f'(sys:load "{PROFILE}")']
    try:
        p = subprocess.run(
            cmd, cwd=REPO, capture_output=True, text=True, timeout=timeout
        )
    except subprocess.TimeoutExpired:
        return "", f"timed out after {timeout} s"
    line = next((l for l in p.stdout.splitlines() if l.startswith("RESULT")), None)
    if line is None:
        errors = [l.strip() for l in p.stdout.splitlines() if "rror" in l][:2]
        return p.stdout, "; ".join(errors) or f"exited with {p.returncode}"
    if "COMPILE-FAILED" in line:
        return p.stdout, "compile failed"
    values: dict[str, float] = {
        "total": float(re.search(r"total=\s*([\d.e+-]+)", line).group(1))
    }
    for name, secs, _calls in re.findall(r"(\S+)\s+=\s+([\d.e+-]+)\s+/\s+(\d+)", line):
        values[PHASES.get(name, name)] = float(secs)
    return p.stdout, values


def exponent(a: tuple[int, float], b: tuple[int, float]) -> str:
    """The growth exponent between two (size, seconds) points, if measurable."""
    (na, ta), (nb, tb) = a, b
    if ta < 0.002 or tb < 0.002:
        return "-"
    return f"{math.log(tb / ta) / math.log(nb / na):.2f}"


def print_table(
    family: str, sizes: list[int], results: dict[int, Result], columns: list[str]
) -> None:
    print(f"\n## {family}: {FAMILIES[family].__doc__}")
    print("n".rjust(6), *(c.rjust(9) for c in columns))
    for n in sizes:
        r = results[n]
        if isinstance(r, str):
            print(str(n).rjust(6), "  ERROR:", r)
        else:
            print(str(n).rjust(6), *(f"{r.get(c, 0.0):9.3f}" for c in columns))
    ok = [n for n in sizes if not isinstance(results[n], str)]
    if len(ok) >= 2:
        a, b = ok[-2], ok[-1]
        ra, rb = results[a], results[b]
        assert not isinstance(ra, str) and not isinstance(rb, str)
        print(
            "exp".rjust(6),
            *(
                exponent((a, ra.get(c, 0.0)), (b, rb.get(c, 0.0))).rjust(9)
                for c in columns
            ),
        )


def cmd_sweep(args: argparse.Namespace) -> int:
    families = args.families or list(FAMILIES)
    unknown = [f for f in families if f not in FAMILIES]
    if unknown:
        sys.exit(f"unknown families: {', '.join(unknown)} (see: stress.py list)")
    with tempfile.TemporaryDirectory(prefix="xtm-stress-") as tmp:
        workdir = Path(tmp)
        share = snapshot(workdir) if args.snapshot else None
        plan = [
            (f, n)
            for f in families
            for n in (SMALL_SIZES.get(f) if args.sizes is None else args.sizes)
            or DEFAULT_SIZES
        ]
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            outcomes = list(
                pool.map(
                    lambda c: run_case(
                        workdir, c[0], c[1], share, args.wrap, args.timeout
                    ),
                    plan,
                )
            )
        results: dict[str, dict[int, Result]] = {}
        for (family, n), (_out, r) in zip(plan, outcomes):
            results.setdefault(family, {})[n] = r
        columns = ["total", *[c for c in PHASES.values() if c != "run"], *args.wrap]
        for family in families:
            print_table(family, sorted(results[family]), results[family], columns)
        return (
            1
            if any(isinstance(r, str) for rs in results.values() for r in rs.values())
            else 0
        )


def cmd_profile(args: argparse.Namespace) -> int:
    with tempfile.TemporaryDirectory(prefix="xtm-stress-") as tmp:
        workdir = Path(tmp)
        share = snapshot(workdir) if args.snapshot else None
        out, r = run_case(workdir, args.family, args.n, share, args.wrap, args.timeout)
        if isinstance(r, str):
            print(out)
            print(f"ERROR: {r}")
            return 1
        for name, secs in r.items():
            print(f"{name:>45} {secs:9.3f}")
        return 0


def cmd_gen(args: argparse.Namespace) -> int:
    setup, timed = FAMILIES[args.family](args.n)
    if setup:
        print(";; setup (untimed)\n" + setup + "\n\n;; timed")
    print(timed)
    return 0


def cmd_list(_args: argparse.Namespace) -> int:
    for name, family in FAMILIES.items():
        print(f"{name:16} {family.__doc__}")
    return 0



def sizes(text: str) -> list[int]:
    return [int(s) for s in text.split(",")]


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = parser.add_subparsers(dest="command", required=True)

    def common(p: argparse.ArgumentParser) -> None:
        p.add_argument(
            "--wrap",
            type=lambda s: s.split(","),
            default=[],
            help="more compiler functions to time, comma-separated",
        )
        p.add_argument(
            "--timeout", type=int, default=180, help="seconds per compile (default 180)"
        )
        p.add_argument(
            "--no-snapshot",
            dest="snapshot",
            action="store_false",
            help="use the live runtime/ rather than a copy",
        )

    p = sub.add_parser("sweep", help="time families across sizes")
    p.add_argument("families", nargs="*", help="families to run (default: all)")
    p.add_argument(
        "--sizes",
        type=sizes,
        default=None,
        help=f"comma-separated sizes (default {','.join(map(str, DEFAULT_SIZES))}; "
        "lambdas defaults to 10,20,40,80)",
    )
    p.add_argument("--jobs", type=int, default=4, help="compiles at once (default 4)")
    common(p)
    p.set_defaults(func=cmd_sweep)

    p = sub.add_parser("profile", help="time one family at one size")
    p.add_argument("family", choices=FAMILIES)
    p.add_argument("n", type=int)
    common(p)
    p.set_defaults(func=cmd_profile)

    p = sub.add_parser("gen", help="print one family's xtlang at one size")
    p.add_argument("family", choices=FAMILIES)
    p.add_argument("n", type=int)
    p.set_defaults(func=cmd_gen)

    p = sub.add_parser("list", help="list the families")
    p.set_defaults(func=cmd_list)

    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
