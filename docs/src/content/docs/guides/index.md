---
title: Guides
description: Longer-form guides for making music and audio with Extempore.
---

These guides go deeper than the [reference](/reference/types/)---each one walks
through a real task end to end. If you're just starting out, set up
[editor support](/guides/editor-support/) first so your editor can talk to a
running Extempore, then pick a track below.

## Making sound from scratch

Build up the audio signal chain yourself, from oscillators to a full instrument:

1. [Audio signal processing](/guides/audio-signal-processing/) --- the `dsp`
   closure, oscillators, and building a `saw_synth` from first principles.
2. [Making an instrument](/guides/making-an-instrument/) --- note and effect
   kernels and `make-instrument`, built around a tonewheel organ.
3. [Sampler](/guides/sampler/) --- playing and pitching audio samples, from a
   drum kit to a sampled piano.

## Playing notes and patterns

Drive instruments at the note level and sequence them over time:

1. [Note-level music](/guides/note-level-music/) --- `play-note`, temporal
   recursion, metronomes, and pitch material.
2. [Pattern language](/guides/pattern-language/) --- the `:>` pattern DSL for
   concise, live-editable sequences.

## Other guides

- [Audio file I/O](/guides/audio-file-io/) --- reading and playing back audio
  files with `AudioBuffer`.
