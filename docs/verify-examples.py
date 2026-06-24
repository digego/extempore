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
                    claim found in the file must appear in the output.

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
BIN = os.path.join(ROOT, "build/extempore")
TIMEOUT = 90

# filename (relative to DOCS) -> verification mode
CONFIG = {
    "reference/error-messages.md": "error-pairs",
    "reference/tutorial.md": "accumulate",
    "reference/types.md": "accumulate",
    "reference/memory-management.md": "accumulate",
}

ANSI = re.compile(r"\x1b\[[0-9;]*m")


def norm(s):
    """Normalise volatile compiler internals so comparisons are stable."""
    s = re.sub(r"_adhoc_[A-Za-z0-9_]+", "_adhoc_*", s)
    s = re.sub(r"##\d+", "##N", s)
    return s.strip()


def run(code):
    """Run xtlang `code` through --batch, return de-ANSI'd combined output."""
    snippet = code if "(quit" in code else code + "\n(quit 0)\n"
    try:
        p = subprocess.run(
            ["timeout", str(TIMEOUT), BIN, "--batch", snippet],
            capture_output=True, text=True, cwd=ROOT, timeout=TIMEOUT + 30,
        )
        out = p.stdout + p.stderr
    except subprocess.TimeoutExpired as e:
        out = (e.stdout or "") + (e.stderr or "")
        if isinstance(out, bytes):
            out = out.decode("utf-8", "replace")
    return ANSI.sub("", out)


def blocks(text):
    """Yield (lang, code) for every fenced block, lang='' for plain fences."""
    fence = re.compile(r"^([`~]{3,})([\w-]*)\s*$")
    lines = text.splitlines()
    i = 0
    while i < len(lines):
        m = fence.match(lines[i])
        if m:
            marker, lang = m.group(1), m.group(2)
            body = []
            i += 1
            while i < len(lines) and not lines[i].startswith(marker[0] * 3):
                body.append(lines[i])
                i += 1
            yield lang, "\n".join(body)
        i += 1


def check_error_pairs(text):
    """Pair each xtlang block with the next plain block (its expected output)."""
    bs = list(blocks(text))
    results = []
    pending = None  # (index, code)
    pairs = []
    for lang, body in bs:
        if lang == "xtlang":
            pending = body
        elif lang == "" and pending is not None:
            pairs.append((pending, body))
            pending = None
    outs = list(EXEC.map(run, [c for c, _ in pairs]))
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


def check_accumulate(text):
    code = "\n".join(b for lang, b in blocks(text) if lang == "xtlang")
    actual = run(code)
    results = []
    claims = []
    for line in text.splitlines():
        m = re.search(r"Compiled:\s+\w+ >>> \S+", line)
        if m:
            claims.append(m.group(0))
        m = re.search(r';;\s*prints\s+"([^"]+)"', line)
        if m:
            claims.append(("prints", m.group(1)))
    for c in claims:
        if isinstance(c, tuple):
            ok = c[1] in actual
            label = f'prints "{c[1]}"'
        else:
            ok = norm(c) in norm(actual)
            label = c
        if not ok:
            results.append((label, ["<not found in output>"], ""))
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
        print(f"\n=== {rel}  [{mode}] ===")
        if mode == "error-pairs":
            for snippet_id, missing, actual in check_error_pairs(text):
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
            results, n = check_accumulate(text)
            for label, missing, _ in results:
                fail += 1
                print(f"  FAIL  claim not in output: {label}")
            print(f"  checked {n} claims, {len(results)} failed")
    print(f"\n{'FAILURES: ' + str(fail) if fail else 'all checked claims verified'}")
    return 1 if fail else 0


if __name__ == "__main__":
    sys.exit(main())
