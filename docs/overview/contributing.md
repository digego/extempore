---
title: Contributing
---

# Contributing to Extempore

There are heaps of ways you can contribute to Extempore---whether you've been
hacking Extempore code for a long time or you're just starting out. If you're
in the latter camp, the docs are an especially good place to help: take notes
as you learn, and open a PR for anything that's missing or unclear. It's hard
for [Andrew](https://github.com/digego) and
[Ben](https://github.com/benswift) to remember what it's like to start from
scratch, so fresh eyes are genuinely valuable.

If you've got questions, or want to bounce ideas around before making a big
change, get in touch on the
[mailing list](mailto:extemporelang@googlegroups.com) or open an issue on
[GitHub](https://github.com/digego/extempore/issues).

## Documentation

These docs are built with [VitePress](https://vitepress.dev). The sources
live in [`docs/`](https://github.com/digego/extempore/tree/master/docs) in
the main repo---just markdown files, easy to edit.

### Editing

1. Fork [`digego/extempore`](https://github.com/digego/extempore).
2. Edit the relevant `.md` file under `docs/`.
3. Open a PR.

There's an "Edit this page on GitHub" link at the bottom of every docs page
that will drop you straight into the right file.

### Building locally

From the repo root:

```sh
cd docs
npm install
npm run dev       # live-reloading dev server
npm run build     # production build into docs/.vitepress/dist
npm run preview   # preview the production build
```

### Style

There's no formal style guide, but a few principles:

- content goes in `overview/`, `reference/` or `guides/` (wherever fits best)
- filenames use kebab-case (e.g. `page-title.md`)
- use level-2 headings (`##`) and below in page bodies---the page title comes
  from the frontmatter
- prefer second-person ("now you've built an instrument") over first-person
  plural ("now we've built an instrument"); some older pages still use "we"
  from their blog-post origins, and we're converting them over time

## Contributing code

The main source tree is C++ (runtime) and xtlang (standard library and
examples). See [`BUILDING.md`](https://github.com/digego/extempore/blob/master/BUILDING.md)
for how to build from source, and [`CLAUDE.md`](https://github.com/digego/extempore/blob/master/CLAUDE.md)
for a one-page map of the codebase.

Issues and PRs live at
[digego/extempore](https://github.com/digego/extempore).

## Extempore wishlist

A few projects (some small, some not so small) that would be really nice. If
any catch your eye, give us a shout on the
[mailing list](mailto:extemporelang@googlegroups.com) before you start.

### xtlang libraries

1. a 2D/3D hardware-accelerated data visualisation library (e.g. a vega-lite
   for Extempore)
2. TensorFlow (or equivalent) C bindings for inference, with an example of a
   deep-learning image-processing model
3. additional graphics backends (DirectX, Vulkan)
4. [Godot](https://godotengine.org)/Extempore integration

### Ecosystem

1. an xtlang package manager (CPAN/cargo-style)
2. make the CMake build aware of the xtlang ahead-of-time compilation
   process, so `make aot` only re-AOT-compiles a library if it has changed
