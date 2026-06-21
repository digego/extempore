# AOT compilation benchmarks

Comparison of ahead-of-time compilation times for all core and external audio
libraries, measured before and after the xtlang compiler refactor and s7 Scheme
migration.

## Setup

- **Platform**: Linux x86_64, kernel 6.8.0-101, AMD Zen 4
- **LLVM**: 22.1.0 with ORC JIT
- **Build**: `cmake -DCMAKE_BUILD_TYPE=Release -DEXTERNAL_SHLIBS_GRAPHICS=OFF`
- **Baseline** (commit `44b7b5c5`): TinyScheme interpreter, pre-refactor
  compiler with assoc-list caches
- **Current** (commit `93dacf79`): s7 Scheme interpreter, refactored compiler
  with hash table caches, union-find type unification, constraint-based solving

Each library is compiled in a separate `extempore --nobase --batch` process.
Wall clock times include ~3s of startup overhead per invocation (hard-coded
`sleep(1) + sleep(2)` in `taskImpl` for NSApp initialisation).

## Results

| Library                             |    Baseline |    Current |  Speedup |
| ----------------------------------- | ----------: | ---------: | -------: |
| `libs/base/base.xtm`                |       5.59s |      3.24s |     1.7x |
| `libs/core/math.xtm`                |      16.57s |      4.04s |     4.1x |
| `libs/core/rational.xtm`            |       6.47s |      3.47s |     1.9x |
| `libs/core/audiobuffer.xtm`         |       7.45s |      3.49s |     2.1x |
| `libs/core/audio_dsp.xtm`           |      32.14s |      5.71s |     5.6x |
| `libs/core/instruments.xtm`         |     109.83s |      8.38s |    13.1x |
| `libs/external/fft.xtm`             |       5.71s |      3.99s |     1.4x |
| `libs/external/sndfile.xtm`         |       9.17s |      3.64s |     2.5x |
| `libs/external/audio_dsp_ext.xtm`   |       5.51s |      4.28s |     1.3x |
| `libs/external/instruments_ext.xtm` |      12.57s |      9.26s |     1.4x |
| `libs/external/portmidi.xtm`        |       4.86s |      3.35s |     1.5x |
| **Total**                           | **215.96s** | **52.94s** | **4.1x** |

Subtracting ~3s startup overhead per invocation (33s total), the pure compile
time went from ~183s to ~20s --- a **9x speedup**.

## Notes

- The largest improvement is `instruments.xtm` (13.1x), which has the heaviest
  generic instantiation and type inference workload.
- Libraries where the speedup is modest (1.3--1.5x) are dominated by startup
  overhead and LLVM JIT time rather than Scheme-level type checking.
- Date: 2 March 2026.
