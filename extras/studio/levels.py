#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["numpy", "scipy"]
# ///
"""Measure how loud each preset of a studio instrument is.

Renders one note (or, for a kit, each sound) with every preset, with the
master compressor switched off, and prints the peak and RMS level of each. An
instrument's presets should sit within a few dB of each other, and of the other
instruments: around -12 dBFS peak for one note at a velocity of 0.8.

    extras/studio/levels.py polysynth
    extras/studio/levels.py kit808
    extras/studio/levels.py epiano --pitch 48 --chord
"""

import argparse
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import numpy as np
from scipy.io import wavfile

ROOT = Path(__file__).resolve().parents[2]
SLOT = 3.0  # seconds given to each note


def clause(text: str, name: str) -> list[str]:
    """The names at the head of each entry in a def-instrument clause."""
    start = text.find(f"({name}")
    if start < 0:
        return []
    names, depth, i = [], 1, start + len(name) + 1
    while i < len(text) and depth > 0:
        char = text[i]
        if char == ";":
            while i < len(text) and text[i] != "\n":
                i += 1
        elif char == "(":
            depth += 1
            if depth == 2:
                match = re.match(r"\(([^\s()]+)", text[i:])
                if match:
                    names.append(match.group(1))
        elif char == ")":
            depth -= 1
        i += 1
    return names


def sound_pitches(text: str) -> dict[str, int]:
    start = text.find("(sounds")
    if start < 0:
        return {}
    end = text.find("))", start)
    return {name: int(pitch) for name, pitch in re.findall(r"\(([\w-]+)\s+(\d+)\)", text[start:end + 2])}


def db(x: float) -> float:
    return 20.0 * np.log10(max(x, 1e-10))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("instrument")
    parser.add_argument("--pitch", type=int, default=60, help="MIDI note to play (default 60)")
    parser.add_argument("--chord", action="store_true", help="play a four-note chord instead of one note")
    parser.add_argument("--velocity", type=float, default=0.8)
    args = parser.parse_args()

    source = (ROOT / "libs/studio/instruments" / f"{args.instrument}.xtm").read_text()
    presets = clause(source, "presets") or ["-"]
    sounds = sound_pitches(source)

    tests: list[tuple[str, str, list[int]]] = []  # (preset, label, pitches)
    for preset in presets:
        if sounds:
            tests += [(preset, sound, [pitch]) for sound, pitch in sounds.items()]
        else:
            pitches = [args.pitch + i for i in (0, 4, 7, 11)] if args.chord else [args.pitch]
            tests.append((preset, "chord" if args.chord else f"note {args.pitch}", pitches))

    lines = ['(sys:load "libs/studio/studio.xtm")', "(score-begin)", f"(track 'x '{args.instrument})",
             "(ctl 'master 'comp-ratio 1.0 'comp-makeup 0.0)"]
    last_preset = None
    for i, (preset, _, pitches) in enumerate(tests):
        at = i * SLOT
        if preset != last_preset and preset != "-":
            lines.append(f"(score-preset {at} 'x '{preset})")
            last_preset = preset
        for pitch in pitches:
            lines.append(f"(score-note {at + 0.05} 'x {pitch} {args.velocity} 1.5)")
    lines.append("(score-end)")

    with tempfile.TemporaryDirectory() as tmp:
        score, wav, log = Path(tmp) / "levels.xtm", Path(tmp) / "levels.wav", Path(tmp) / "levels.log"
        score.write_text("\n".join(lines) + "\n")
        with log.open("w") as out:
            subprocess.run(
                ["build/extempore", "render", str(score), "-o", str(wav),
                 "--duration", str(len(tests) * SLOT)],
                cwd=ROOT, stdout=out, stderr=subprocess.STDOUT, timeout=900, check=False)
        errors = [line for line in log.read_text().splitlines() if "rror" in line or line.startswith(";")]
        if errors:
            print("the log has errors:\n  " + "\n  ".join(errors[:10]))
        if not wav.exists():
            sys.exit("no audio was rendered")
        rate, data = wavfile.read(wav)

    data = np.atleast_2d(data.astype(np.float64).T).T
    print(f"{args.instrument}: {'each sound' if sounds else tests[0][1]}, velocity {args.velocity}")
    print(f"  {'preset':<14}{'sound':<10}{'peak dBFS':>10}{'rms dBFS':>10}   (rms over the first second)")
    peaks = []
    for i, (preset, label, _) in enumerate(tests):
        segment = data[int(i * SLOT * rate):int((i + 1) * SLOT * rate)]
        if len(segment) == 0:
            continue
        peak = float(np.max(np.abs(segment)))
        first = segment[int(0.05 * rate):int(1.05 * rate)]
        rms = float(np.sqrt(np.mean(first**2))) if len(first) else 0.0
        peaks.append(db(peak))
        flag = "  silent!" if peak < 1e-4 else ("  hot" if peak > 0.5 else "")
        print(f"  {preset:<14}{label if sounds else '':<10}{db(peak):>10.1f}{db(rms):>10.1f}{flag}")
    if peaks:
        print(f"  peaks range from {min(peaks):.1f} to {max(peaks):.1f} dBFS (median {np.median(peaks):.1f})")


if __name__ == "__main__":
    main()
