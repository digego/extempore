---
title: Contributing
---

# Contributing to Extempore

There are heaps of ways you can contribute to Extempore---whether you've been
hacking Extempore code for a long time or you're just starting out. If you're in
the latter camp, the docs are an especially good place to help: take notes as
you learn, and open a PR for anything that's missing or unclear. It's hard for
[Andrew](https://github.com/digego) and [Ben](https://github.com/benswift) to
remember what it's like to start from scratch, so fresh eyes are genuinely
valuable.

If you've got questions, or want to bounce ideas around before making a big
change, get in touch on the
[mailing list](mailto:extemporelang@googlegroups.com) or open an issue on
[GitHub](https://github.com/digego/extempore/issues).

## Documentation

These docs are built with [Astro Starlight](https://starlight.astro.build). The
sources live in [`docs/`](https://github.com/digego/extempore/tree/master/docs)
in the main repo---just markdown files, easy to edit.

### Editing

1. Fork [`digego/extempore`](https://github.com/digego/extempore).
2. Edit the relevant `.md` file under `docs/`.
3. Open a PR.

There's an "Edit this page on GitHub" link at the bottom of every docs page that
will drop you straight into the right file.

### Building locally

From the repo root:

```sh
cd docs
npm install
npm run dev       # live-reloading dev server
npm run build     # production build into docs/dist
npm run preview   # preview the production build
```

### Verifying code snippets

The xtlang code in the reference pages (and some of the guides) is checked
against the real compiler by `docs/verify-examples.py`, which runs each snippet
through `extempore --batch` and compares the output to what the docs claim (a
`Compiled:  NAME >>> SIG` line, a `;; prints "..."` comment, or---for
`error-messages.md`---the error block that follows the snippet). A claim is only
checked when a block that actually runs would produce it: a
`Compiled:`/`;; prints` line inside a runnable block, or in the output block
right after one. Claims in prose, or after a skipped block, are
ignored---they're illustration, not output. (`--batch` echoes each snippet back
before running it, so the verifier subtracts that echo first and matches claims
only against what the program actually printed---otherwise a `;; prints "..."`
comment would match itself.)

It's a **local tool**, not part of CI: it needs a built `extempore` and a real
run takes a few minutes (snippets that deliberately error don't exit under
`--batch`, so each waits out a short timeout). Run it on Linux against your
build before changing the docs:

```sh
# either directly...
EXTEMPORE_BIN=./build/extempore python3 docs/verify-examples.py
# ...or via the (CI-excluded) ctest label
ctest --test-dir build -L docs --output-on-failure
```

Two HTML-comment directives (invisible in the rendered page) tell the verifier
how to treat a block that can't just be run and checked:

- `<!-- verify: expect-error -->` --- the block is _meant_ to fail to compile.
  It's run on top of the preceding blocks and the verifier asserts it really
  errors and that the error block shown matches; it's also kept out of the
  shared session so it can't derail later snippets.
- `<!-- verify: skip -->` --- the block can't be auto-run headless (it needs the
  audio device or the sharedsystem, or it's an illustrative fragment). It's left
  out of the run entirely.

Put the comment on its own line immediately before the block's opening fence. If
you add or change a reference snippet, run the verifier locally before opening
the PR.

### Style

There's no formal style guide, but a few principles:

- content goes in `overview/`, `reference/` or `guides/` (wherever fits best)
- filenames use kebab-case (e.g. `page-title.md`)
- use level-2 headings (`##`) and below in page bodies---the page title comes
  from the frontmatter
- prefer second-person ("now you've built an instrument") over first-person
  plural ("now we've built an instrument"); some older pages still use "we" from
  their blog-post origins, and we're converting them over time

## Contributing code

The main source tree is C++ (runtime) and xtlang (standard library and
examples). See
[`BUILDING.md`](https://github.com/digego/extempore/blob/master/BUILDING.md) for
how to build from source, and
[`CLAUDE.md`](https://github.com/digego/extempore/blob/master/CLAUDE.md) for a
one-page map of the codebase.

Issues and PRs live at [digego/extempore](https://github.com/digego/extempore).

## Extempore wishlist

A few projects (some small, some not so small) that would be really nice. If any
catch your eye, give us a shout on the
[mailing list](mailto:extemporelang@googlegroups.com) before you start.

### xtlang libraries

1. a 2D/3D hardware-accelerated data visualisation library (e.g. a vega-lite for
   Extempore)
2. TensorFlow (or equivalent) C bindings for inference, with an example of a
   deep-learning image-processing model
3. additional graphics backends (DirectX, Vulkan)
4. [Godot](https://godotengine.org)/Extempore integration

### Ecosystem

1. an xtlang package manager (CPAN/cargo-style)
2. make the CMake build aware of the xtlang ahead-of-time compilation process,
   so `make aot` only re-AOT-compiles a library if it has changed
