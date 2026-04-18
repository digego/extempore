---
title: Contributing
---

There are heaps of ways you can contribute to Extempore---whether you've been
hacking Extempore code for a long time or whether you're just starting out. If
you're in the latter category then you can especially help out with the
docs---take notes as you learn, write down (& suggest) fixes for anything that's
missing or unclear. It's hard for [Andrew](https://twitter.com/digego) &
[Ben](https://twitter.com/benswift) to remember what it's like to start out, but
that doesn't mean that others should have to fight through like we did :)

If you've got questions, or want to bounce around some ideas for improvements
before you go ahead and make big changes then get in touch on the [mailing
list](mailto:extemporelang@googlegroups.com).

## Documentation

This documentation is built using [Jekyll](https://jekyllrb.com/), using the
[Jekyll Doc](https://aksakalli.github.io/jekyll-doc-theme/) theme. Since it's
all just markdown files in the `_docs/` subdirectory, it's easy for others to
contribute.

If you find problems, or can think of improvements, [fork away on
GH](https://github.com/extemporelang/extempore), edit the documentation source files
and submit a pull request---there's a nice little "IMPROVE THIS PAGE" link at the
bottom of every page. We'd love these docs to become a real community effort.
There will probably be a few broken links and other little things like that, so
no pull request is too small to be appreciated :)

### Building

To generate these docs, you'll need a working ruby install & a few packages. The
best way to get started is to use [bundler](http://bundler.io/), then it's:

```
    bundle install # to get all the packages
    bundle exec jekyll build # to build the docs website
```

If you want to see your changes locally (which of course you do!) then you can
run a local 'live' test server with

```
    bundle exec jekyll serve              # or
    bundle exec jekyll serve --livereload # to have the site reload on save
```

### Hosting

The docs are built and hosted automatically by GitHub through [GitHub
pages](https://pages.github.com/). So the docs are re-built for every commit to
the master branch of [the extemporelang.github.io
repo](https://github.com/extemporelang/extemporelang.github.io).

### Style {#documentation-style}

There's no official styleguide, and as mentioned elsewhere some of this started
off as blog posts on Ben's blog, so it's a bit all-over-the-place when it comes
to style. Still, here are some general style/formatting principles:

- on each page, use level 2 headings (`##`) as the highest level (i.e. no level
  1 headings---that's reserved for the page title)

- content should go in either the `overview`, `reference` or `guides` folders
  (wherever it fits best)

- use kebab-case for docs filenames (e.g. `page-title.md`)

- a conversational writing style is ok, preferrably in a second-person narrative
  voice (e.g. "now you're built an instrument") rather than first-person ("now
  we've built an instrument") (**note:** there's a bunch of "we" stuff in there
  from when Ben first wrote the material as blog posts, but the plan is to
  change it to "you" over time)

## Extempore wishlist

Building a new programming language, runtime and ecosystem is a multifaceted
job. Here are a few projects (some small, some not so small) which would be
really nice---if you think you'd like to contribute, give us a shout out on the
[mailing list](mailto:extemporelang@googlegroups.com).

### Core {#core}

These projects involve hacking on the Extempore executable itself:

1. upgrade to latest LLVM with ORCJIT
2. port Extempore to 64-bit ARM (`aarch64`)

### xtlang {#xtlang}

These projects (mostly) involve adding/improving libraries for doing cool things
in xtlang:

1. a 2D/3D hardware-accelerated data visualisation library (e.g. a vega-lite
   for Extempore)
2. add TensorFlow C bindings for (at least) inference, & an example of how to
   run a cool deep-learning-powered image processing model
3. add DirectX (or perhaps Vulkan) support
4. implement [Godot](https://godotengine.org)/Extempore integration

### Ecosystem {#ecosystem}

These projects are "ecosystem/tooling" projects.

1. create an xtlang package manager (e.g. CPAN or cargo for Extempore)
2. make the CMake build process aware of the xtlang ahead-of-time compilation
   process, so that `make aot` only re-aot-compiles an xtlang library if it has
   changed
