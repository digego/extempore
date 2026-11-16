#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = ["numpy", "scipy", "matplotlib"]
# ///
"""Describe a rendered WAV file in numbers and pictures.

Nobody is listening when the studio's instruments are rendered offline, so
this prints what a listener would notice first: is there any sound, is it
clipping, is it lopsided, where is its energy, what pitch is it, and how does
its level move over time. With --png it also draws a spectrogram.

    extras/studio/analyse.py render.wav --png render.png
"""

import argparse
import sys
from pathlib import Path

import numpy as np
from scipy import signal
from scipy.io import wavfile

NOTE_NAMES = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]


def db(x: float) -> float:
    return 20.0 * np.log10(max(x, 1e-10))


def note_name(freq: float) -> str:
    if freq <= 0.0:
        return "-"
    midi = 69.0 + 12.0 * np.log2(freq / 440.0)
    nearest = int(round(midi))
    cents = int(round(100.0 * (midi - nearest)))
    return f"{NOTE_NAMES[nearest % 12]}{nearest // 12 - 1}{cents:+d}c"


def pitch_of(x: np.ndarray, sr: int) -> float:
    """Fundamental frequency by autocorrelation, or 0.0 if there isn't a clear one."""
    x = x - np.mean(x)
    if np.max(np.abs(x)) < 1e-4:
        return 0.0
    n = len(x)
    spectrum = np.fft.rfft(x * np.hanning(n), 2 * n)
    acf = np.fft.irfft(np.abs(spectrum) ** 2)[:n]
    acf /= acf[0] + 1e-12
    lo, hi = int(sr / 5000), min(int(sr / 25), n - 1)
    if hi <= lo:
        return 0.0
    lag = lo + int(np.argmax(acf[lo:hi]))
    if acf[lag] < 0.5:
        return 0.0
    # the first strong peak, so that we report the fundamental and not a subharmonic
    peaks, _ = signal.find_peaks(acf[lo:hi], height=0.85 * acf[lag])
    if len(peaks):
        lag = lo + int(peaks[0])
    if 0 < lag < n - 1:
        a, b, c = acf[lag - 1], acf[lag], acf[lag + 1]
        denominator = a - 2 * b + c
        offset = 0.5 * (a - c) / denominator if denominator != 0 else 0.0
        return sr / (lag + offset)
    return sr / lag


def sparkline(values: np.ndarray, floor: float = -60.0) -> str:
    blocks = " ▁▂▃▄▅▆▇█"
    scaled = np.clip((values - floor) / -floor, 0.0, 1.0)
    return "".join(blocks[int(round(v * (len(blocks) - 1)))] for v in scaled)


def band_energy(mono: np.ndarray, sr: int) -> list[tuple[str, float]]:
    freqs, psd = signal.welch(mono, sr, nperseg=min(8192, len(mono)))
    total = np.sum(psd) + 1e-20
    bands = [("sub <60", 0, 60), ("bass 60-250", 60, 250), ("low-mid 250-1k", 250, 1000),
             ("high-mid 1k-4k", 1000, 4000), ("presence 4k-10k", 4000, 10000), ("air >10k", 10000, sr)]
    return [(name, 100.0 * np.sum(psd[(freqs >= lo) & (freqs < hi)]) / total) for name, lo, hi in bands]


def analyse(path: Path, png: Path | None, start: float, length: float | None) -> int:
    sr, data = wavfile.read(path)
    if data.dtype.kind == "i":
        data = data.astype(np.float64) / np.iinfo(data.dtype).max
    data = np.atleast_2d(data.astype(np.float64).T).T
    first = int(start * sr)
    last = len(data) if length is None else min(len(data), first + int(length * sr))
    data = data[first:last]
    frames, channels = data.shape
    problems = []

    bad = int(np.sum(~np.isfinite(data)))
    if bad:
        problems.append(f"{bad} samples are NaN or infinite")
        data = np.nan_to_num(data, nan=0.0, posinf=0.0, neginf=0.0)
    mono = np.mean(data, axis=1)
    peak = float(np.max(np.abs(data))) if frames else 0.0
    rms = float(np.sqrt(np.mean(data**2))) if frames else 0.0

    print(f"{path.name}: {frames / sr:.2f} s, {channels} ch, {sr} Hz")
    print(f"  peak {db(peak):6.1f} dBFS   rms {db(rms):6.1f} dBFS   crest {db(peak) - db(rms):4.1f} dB")
    if peak < 1e-4:
        problems.append("silent")
    if peak >= 0.999:
        clipped = int(np.sum(np.abs(data) >= 0.999))
        problems.append(f"clipping: {clipped} samples at or over full scale")
    dc = float(np.max(np.abs(np.mean(data, axis=0)))) if frames else 0.0
    if dc > 0.01:
        problems.append(f"DC offset of {dc:.3f}")
    if channels >= 2 and peak > 1e-4:
        left, right = data[:, 0], data[:, 1]
        balance = db(np.sqrt(np.mean(left**2))) - db(np.sqrt(np.mean(right**2)))
        denominator = np.sqrt(np.sum(left**2) * np.sum(right**2)) + 1e-20
        correlation = float(np.sum(left * right) / denominator)
        print(f"  stereo: L-R balance {balance:+.1f} dB, correlation {correlation:+.2f} "
              "(+1 mono, 0 wide, negative: phase trouble)")
        if abs(balance) > 6.0:
            problems.append(f"lopsided stereo image ({balance:+.1f} dB)")
        if correlation < -0.3:
            problems.append("left and right are out of phase")

    if peak > 1e-4:
        freqs, psd = signal.welch(mono, sr, nperseg=min(8192, len(mono)))
        centroid = float(np.sum(freqs * psd) / (np.sum(psd) + 1e-20))
        print(f"  spectral centroid {centroid:.0f} Hz")
        print("  energy: " + ", ".join(f"{name} {share:.0f}%" for name, share in band_energy(mono, sr)))

        # level and pitch over time, in a fixed number of slices
        slices = 48
        size = max(1, frames // slices)
        levels = np.array([db(float(np.sqrt(np.mean(mono[i * size:(i + 1) * size] ** 2))))
                           for i in range(slices) if (i + 1) * size <= frames])
        print(f"  level over time ({size / sr:.2f} s per step, floor -60 dB):")
        print(f"    |{sparkline(levels)}|")
        window = min(frames, int(0.1 * sr))
        steps = 12
        pitches = []
        for i in range(steps):
            at = int(i * (frames - window) / max(1, steps - 1))
            pitches.append(note_name(pitch_of(mono[at:at + window], sr)))
        print("  pitch at 12 points: " + " ".join(pitches))
        tail = mono[-min(frames, int(0.05 * sr)):]
        tail_level = db(float(np.sqrt(np.mean(tail**2))))
        print(f"  level in the last 50 ms: {tail_level:.1f} dBFS")

    if png is not None and peak > 1e-4:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        figure, (top, bottom) = plt.subplots(2, 1, figsize=(12, 7), height_ratios=[1, 3], sharex=True)
        times = np.arange(frames) / sr
        top.plot(times, mono, linewidth=0.4, color="#2a6f97")
        top.set_ylim(-1.05, 1.05)
        top.set_ylabel("amplitude")
        f, t, sxx = signal.spectrogram(mono, sr, nperseg=2048, noverlap=1536)
        bottom.pcolormesh(t, f, 10 * np.log10(sxx + 1e-12), vmin=-110, vmax=-20, cmap="magma", shading="auto")
        bottom.set_yscale("symlog", linthresh=200)
        bottom.set_ylim(20, sr / 2)
        bottom.set_yticks([50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000])
        bottom.set_yticklabels(["50", "100", "200", "500", "1k", "2k", "5k", "10k", "20k"])
        bottom.set_ylabel("Hz")
        bottom.set_xlabel("seconds")
        figure.suptitle(path.name)
        figure.tight_layout()
        figure.savefig(png, dpi=80)
        print(f"  spectrogram: {png}")

    for problem in problems:
        print(f"  PROBLEM: {problem}")
    return 1 if problems else 0


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("wav", type=Path)
    parser.add_argument("--png", type=Path, help="write a waveform and spectrogram here")
    parser.add_argument("--start", type=float, default=0.0, help="skip this many seconds")
    parser.add_argument("--length", type=float, help="analyse only this many seconds")
    args = parser.parse_args()
    sys.exit(analyse(args.wav, args.png, args.start, args.length))


if __name__ == "__main__":
    main()
