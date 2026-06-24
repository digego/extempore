#!/usr/bin/env python3
"""Verify that xtlang code in the docs produces the output the docs claim.

Runs doc code snippets through `extempore --batch` and checks their claimed
output against what the compiler actually emits. Two kinds of claim are checked:

  * error-pairs  -- each ```xtlang snippet is paired with the plain ``` block
                    that follows it (its "Error"/output block); the snippet is
                    run in isolation and every non-blank expected line must
                    appear in the actual output.  Used for error-messages.md.
  * accumulate   -- all ```xtlang blocks in the file are run together in one
                    session (later blocks depend on earlier ones); every
                    `Compiled:  NAME >>> SIG` line and every `;; prints "..."`
                    claim found in the file must appear in the output, and the
                    accumulated session must not raise an unexpected compiler
                    error.

Per-block directives
--------------------
A block can be annotated with an HTML comment immediately before its opening
fence (invisible in the rendered docs):

  <!-- verify: expect-error -->  the block is *meant* to fail to compile. It is
                                 left out of the accumulated session (so it
                                 can't poison later blocks) and instead run in
                                 isolation, where the verifier asserts it really
                                 does error and that the plain block following it
                                 matches the error message shown.

  <!-- verify: skip -->          the block can't be auto-run headless (needs the
                                 audio device, the sharedsystem, or is an
                                 illustrative fragment). It is excluded from the
                                 session and not checked.

Internal mangled names vary run-to-run, so `_adhoc_<stuff>` and `##<n>` are
normalised before comparing.

Usage:  python3 docs/verify-examples.py [file ...]
Exit status is non-zero if any claim fails.
"""
import os
import re
import sys
import subprocess
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DOCS = os.path.join(ROOT, "docs/src/content/docs")
# the built extempore: $EXTEMPORE_BIN (set by the ctest target) or build/extempore
BIN = os.environ.get("EXTEMPORE_BIN") or os.path.join(ROOT, "build/extempore")
TIMEOUT = 90
# a snippet that errors prints its message within a second or two and then, in
# --batch, hangs awaiting input rather than exiting (see error-messages.md). For
# the error/expect-error snippets we only need that message, so cap them short
# instead of waiting out the full timeout on every one.
ERROR_TIMEOUT = 20

# filename (relative to DOCS) -> verification mode
CONFIG = {
    "reference/error-messages.md": "error-pairs",
    "reference/tutorial.md": "accumulate",
    "reference/types.md": "accumulate",
    "reference/memory-management.md": "accumulate",
    "guides/audio-file-io.md": "accumulate",
    "guides/audio-signal-processing.md": "accumulate",
    "guides/making-an-instrument.md": "accumulate",
}

# Deliberately *not* covered (sharedsystem / pattern-language guides):
#   guides/note-level-music.md, guides/sampler.md, guides/pattern-language.md,
#   guides/analogue-synth.md
# These centre on `play-note`/pattern scheduling, which needs the audio clock to
# actually advance --- nothing audible (or assertable) happens in --batch. They
# also load the sharedsystem (examples/sharedsystem/audiosetup.xtm): it *does*
# load headless, but the `Compiled:  NAME >>> SIG` lines the guides quote come
# from a cold compile, whereas a built tree serves those libs from the AOT cache
# and never reprints them, so the claims can never match. Sample loading also
# fails without a real device ("Not a valid SNDFILE* pointer"). Covering these
# would need a headless setup shim that drives the clock and disables the AOT
# cache --- out of scope here; left to a future task if the guides grow runnable,
# self-contained snippets.

ANSI = re.compile(r"\x1b\[[0-9;]*m")
DIRECTIVE = re.compile(r"<!--\s*verify:\s*(expect-error|skip)\s*-->")
# strong markers that a compile genuinely failed (not just prose mentioning them)
ERROR_MARKER = re.compile(r"Compiler Error|Type Error|Could not resolve types")


def norm(s):
    """Normalise volatile compiler internals so comparisons are stable."""
    s = re.sub(r"_adhoc_[A-Za-z0-9_]+", "_adhoc_*", s)
    s = re.sub(r"##\d+", "##N", s)
    s = re.sub(r"!infer_\d+", "!infer_N", s)
    return s.strip()


def run(code, timeout_s=TIMEOUT):
    """Run xtlang `code` through --batch, return de-ANSI'd combined output."""
    snippet = code if "(quit" in code else code + "\n(quit 0)\n"
    try:
        p = subprocess.run(
            ["timeout", str(timeout_s), BIN, "--batch", snippet],
            capture_output=True, text=True, cwd=ROOT, timeout=timeout_s + 30,
        )
        out = p.stdout + p.stderr
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or "") + (e.stderr or "")
        if isinstance(out, bytes):
            out = out.decode("utf-8", "replace")
    return ANSI.sub("", out)


def blocks(text):
    """Yield {lang, code, directive} for every fenced block.

    `directive` is the verify directive ('expect-error'/'skip') from an HTML
    comment immediately preceding the block, else None.
    """
    fence = re.compile(r"^([`~]{3,})([\w-]*)\s*$")
    lines = text.splitlines()
    i = 0
    pending = None
    while i < len(lines):
        dm = DIRECTIVE.search(lines[i])
        if dm:
            pending = dm.group(1)
            i += 1
            continue
        m = fence.match(lines[i])
        if m:
            marker, lang = m.group(1), m.group(2)
            body = []
            i += 1
            while i < len(lines) and not lines[i].startswith(marker[0] * 3):
                body.append(lines[i])
                i += 1
            yield {"lang": lang, "code": "\n".join(body), "directive": pending}
            pending = None
            i += 1
            continue
        if lines[i].strip():
            pending = None  # a directive only attaches to the block right after it
        i += 1


def _error_pairs(bs):
    """Pair each xtlang block with the plain block that follows it."""
    pairs = []
    pending = None
    for b in bs:
        if b["lang"] == "xtlang":
            pending = b
        elif b["lang"] == "" and pending is not None:
            pairs.append((pending, b["code"]))
            pending = None
    return pairs


def check_error_pairs(bs):
    """Run each (xtlang, expected-output) pair in isolation and diff."""
    pairs = [(b["code"], expected) for b, expected in _error_pairs(bs)
             if b["directive"] != "skip"]
    outs = list(EXEC.map(lambda c: run(c, ERROR_TIMEOUT), [c for c, _ in pairs]))
    results = []
    for (code, expected), actual in zip(pairs, outs):
        missing = []
        for line in expected.splitlines():
            if not line.strip():
                continue
            if norm(line) not in norm(actual):
                missing.append(line)
        snippet_id = code.strip().splitlines()[0][:60]
        results.append((snippet_id, missing, actual if missing else ""))
    return results


def _next_plain(bs, idx):
    """The plain output block following block `idx` (its expected output)."""
    for nb in bs[idx + 1:]:
        if nb["lang"] == "":
            return nb["code"]
        if nb["lang"] == "xtlang":
            break
    return ""


def check_expect_errors(bs):
    """Each expect-error block: assert it errors and matches the shown message.

    The block is run on top of the preceding runnable blocks (the deliberate
    error usually depends on earlier definitions), but is itself kept out of the
    accumulated session so it can't poison later blocks.
    """
    prefix, tasks = [], []
    for idx, b in enumerate(bs):
        if b["lang"] != "xtlang":
            continue
        if b["directive"] == "expect-error":
            sid = b["code"].strip().splitlines()[0][:60]
            code = "\n".join(prefix + [b["code"]])
            tasks.append((sid, _next_plain(bs, idx), code))
        elif b["directive"] != "skip":
            prefix.append(b["code"])
    # like the error pairs, these hang after erroring; give them a little more
    # headroom than a bare snippet since they compile the preceding blocks first
    outs = list(EXEC.map(lambda c: run(c, 2 * ERROR_TIMEOUT), [c for _, _, c in tasks]))
    results = []
    for (sid, expected, _), actual in zip(tasks, outs):
        missing = []
        if not ERROR_MARKER.search(actual):
            missing.append("<expected a compiler error, but none was raised>")
        for line in expected.splitlines():
            if line.strip() and norm(line) not in norm(actual):
                missing.append(line)
        results.append((sid, missing, actual if missing else ""))
    return results


def _scan_claims(text):
    """Harvest `Compiled: NAME >>> SIG` and `;; prints "..."` claims from text."""
    claims = []
    for line in text.splitlines():
        m = re.search(r"Compiled:\s+\w+ >>> \S+", line)
        if m:
            claims.append(m.group(0))
        m = re.search(r';;\s*prints\s+"([^"]+)"', line)
        if m:
            claims.append(("prints", m.group(1)))
    return claims


def claims_in_blocks(bs):
    """Harvest claims block-aware: only from output that a runnable block produces.

    A `Compiled: NAME >>> SIG` (or `;; prints "..."`) claim is trusted only when
    the block that would emit it actually runs in the accumulated session ---
    either a `;;`-comment line inside a runnable xtlang block, or a plain output
    block immediately following one. Claims sitting in prose, or after a skipped
    or expect-error block, are ignored: nothing in the session emits them, so
    harvesting them would manufacture false 'not found' failures (the guides'
    illustrative `osc_c`/`tuple_maker` output blocks were exactly this).
    """
    claims = []
    prev_runnable = False
    for b in bs:
        if b["lang"] == "xtlang":
            runnable = b["directive"] not in ("skip", "expect-error")
            if runnable:
                claims += _scan_claims(b["code"])
            prev_runnable = runnable
        elif b["lang"] == "" and prev_runnable:
            claims += _scan_claims(b["code"])
            prev_runnable = False
        else:
            prev_runnable = False
    return claims


def check_accumulate(bs):
    """Run all non-skip, non-expect-error xtlang blocks in one session."""
    runnable = [b["code"] for b in bs
                if b["lang"] == "xtlang" and b["directive"] not in ("skip", "expect-error")]
    actual = run("\n".join(runnable))
    results = []
    claims = claims_in_blocks(bs)
    for c in claims:
        if isinstance(c, tuple):
            ok = c[1] in actual
            label = f'prints "{c[1]}"'
        else:
            ok = norm(c) in norm(actual)
            label = c
        if not ok:
            results.append((label, ["<not found in output>"], ""))
    # the accumulated session excludes the deliberate-error blocks, so any
    # compiler error left in the output is a genuine regression
    err = ERROR_MARKER.search(actual)
    if err:
        ctx = [l for l in actual.splitlines() if ERROR_MARKER.search(l)]
        results.append(("unexpected compiler error in accumulated run",
                        ctx[:4] or ["<error>"], actual))
    # deliberate-error blocks: verify they really fail (keep only the ones that
    # didn't error as claimed --- a clean pass returns empty `missing`)
    results += [(sid, miss, act) for sid, miss, act in check_expect_errors(bs) if miss]
    return results, len(claims)


EXEC = ThreadPoolExecutor(max_workers=8)


def main():
    targets = sys.argv[1:] or list(CONFIG)
    fail = 0
    for rel in targets:
        rel = rel.replace(DOCS + "/", "")
        mode = CONFIG.get(rel)
        path = os.path.join(DOCS, rel)
        if not mode or not os.path.exists(path):
            print(f"SKIP {rel} (no config / missing)")
            continue
        text = open(path, encoding="utf-8").read()
        bs = list(blocks(text))
        print(f"\n=== {rel}  [{mode}] ===", flush=True)
        if mode == "error-pairs":
            for snippet_id, missing, actual in check_error_pairs(bs):
                if missing:
                    fail += 1
                    print(f"  FAIL  {snippet_id}")
                    for m in missing:
                        print(f"        expected line not found: {m!r}")
                    print("        --- actual (tail) ---")
                    for l in actual.strip().splitlines()[-6:]:
                        print(f"        | {l}")
                else:
                    print(f"  ok    {snippet_id}")
        else:
            results, n = check_accumulate(bs)
            for label, missing, actual in results:
                fail += 1
                print(f"  FAIL  {label}")
                for m in missing:
                    print(f"        {m!r}")
                if actual:
                    print("        --- actual (tail) ---")
                    for l in actual.strip().splitlines()[-6:]:
                        print(f"        | {l}")
            print(f"  checked {n} claims, {len(results)} issue(s)")
    print(f"\n{'FAILURES: ' + str(fail) if fail else 'all checked claims verified'}")
    return 1 if fail else 0


if __name__ == "__main__":
    sys.exit(main())
