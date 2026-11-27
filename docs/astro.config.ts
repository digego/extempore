import { defineConfig } from "astro/config";
import { satteri } from "@astrojs/markdown-satteri";
import starlight from "@astrojs/starlight";
import { defineMdastPlugin } from "satteri";

// Honour `## Heading {#custom-id}` syntax (supported by the old VitePress site,
// not by Astro out of the box): strip the marker and set the heading's id.
const headingIds = defineMdastPlugin({
  name: "heading-ids",
  heading(node, ctx) {
    const last = node.children[node.children.length - 1];
    if (last?.type !== "text") return;
    const m = last.value.match(/\s*\{#([\w-]+)\}\s*$/);
    if (!m) return;
    ctx.setProperty(last, "value", last.value.slice(0, last.value.length - m[0].length));
    // `data` is an open bag on the wire; the declared HeadingData type doesn't
    // include the hProperties the mdast -> hast conversion reads.
    const data = node.data as { hProperties?: Record<string, unknown> } | undefined;
    ctx.setProperty(node, "data", {
      ...data,
      hProperties: { ...data?.hProperties, id: m[1] },
    });
  },
});

// https://astro.build/config
export default defineConfig({
  // Astro 7's default ("jsx") drops the line break between wrapped prose and
  // an inline element, running words into links. `true` collapses it to a space.
  compressHTML: true,
  markdown: {
    processor: satteri({ mdastPlugins: [headingIds] }),
  },
  image: {
    // The screenshots are up to 2800px wide but render in a ~800px content
    // column; a constrained layout emits a srcset so narrow viewports don't
    // pay for the full-width source.
    layout: "constrained",
  },
  site: "https://extemporelang.github.io",
  base: "/",
  redirects: {
    // type-inference was folded into the types reference page
    "/reference/type-inference/": "/reference/types/#type-inferencing",
    // the Jekyll site's pages were under /docs/
    ...Object.fromEntries(
      [
        "api/audio_dsp",
        "api/core",
        "guides",
        "guides/analogue-synth",
        "guides/audio-file-io",
        "guides/audio-signal-processing",
        "guides/editor-support",
        "guides/making-an-instrument",
        "guides/note-level-music",
        "guides/pattern-language",
        "guides/sampler",
        "overview/contributing",
        "overview/philosophy",
        "overview/quickstart",
        "overview/time",
        "overview/using-extempore",
        "reference",
        "reference/c-xtlang-interop",
        "reference/concurrency",
        "reference/docstrings",
        "reference/memory-management",
        "reference/scheme-xtlang-interop",
        "reference/testing",
        "reference/types",
      ].map((page) => [`/docs/${page}/`, `/${page}/`]),
    ),
    "/docs/": "/",
    "/docs/guides/common-lisp-music/": "/guides/",
    "/docs/guides/impromptu-users/": "/guides/",
    "/docs/overview/bits-and-pieces/": "/overview/using-extempore/",
    "/docs/reference/type-inference/": "/reference/types/#type-inferencing",
    ...Object.fromEntries(
      [
        "advanced-functions",
        "advanced-types",
        "basics",
        "closures",
        "control-logic",
        "generics",
        "intro-types",
        "memory-management",
        "memory-techiques",
        "strings",
      ].map((page) => [`/docs/reference/new/${page}/`, "/reference/tutorial/"]),
    ),
  },
  integrations: [
    starlight({
      // /404 is owned by src/pages/404.astro --- see the note there.
      disable404Route: true,
      title: "Extempore",
      favicon: "/favicon.ico",
      description: "Livecoding environment for music and audio",
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/digego/extempore",
        },
      ],
      editLink: {
        baseUrl: "https://github.com/digego/extempore/edit/master/docs/",
      },
      // xtlang and extempore code fences are highlighted as Scheme.
      expressiveCode: {
        shiki: {
          langAlias: {
            xtlang: "scheme",
            extempore: "scheme",
          },
        },
      },
      sidebar: [
        {
          label: "Start here",
          items: [
            { label: "Quickstart", slug: "overview/quickstart" },
            { label: "Using Extempore", slug: "overview/using-extempore" },
            { label: "Philosophy", slug: "overview/philosophy" },
            { label: "Contributing", slug: "overview/contributing" },
          ],
        },
        {
          label: "Learn xtlang",
          items: [
            { label: "xtlang tutorial", slug: "reference/tutorial" },
            { label: "Time and scheduling", slug: "overview/time" },
          ],
        },
        {
          label: "xtlang reference",
          items: [
            { label: "Overview", slug: "reference" },
            { label: "Types", slug: "reference/types" },
            { label: "Memory management", slug: "reference/memory-management" },
            { label: "Concurrency", slug: "reference/concurrency" },
            { label: "Scheme-xtlang interop", slug: "reference/scheme-xtlang-interop" },
            { label: "C-xtlang interop", slug: "reference/c-xtlang-interop" },
            { label: "Error messages", slug: "reference/error-messages" },
            { label: "Docstrings", slug: "reference/docstrings" },
            { label: "Testing", slug: "reference/testing" },
          ],
        },
        {
          label: "Guides",
          items: [
            { label: "Overview", slug: "guides" },
            { label: "Editor support", slug: "guides/editor-support" },
            { label: "The nREPL server", slug: "guides/nrepl" },
            { label: "The studio", slug: "guides/studio" },
            { label: "Pattern language", slug: "guides/pattern-language" },
            { label: "Analogue synth", slug: "guides/analogue-synth" },
            { label: "Audio signal processing", slug: "guides/audio-signal-processing" },
            { label: "Audio file I/O", slug: "guides/audio-file-io" },
            { label: "Making an instrument", slug: "guides/making-an-instrument" },
            { label: "MIDI", slug: "guides/midi" },
            { label: "Note-level music", slug: "guides/note-level-music" },
            { label: "Sampler", slug: "guides/sampler" },
          ],
        },
        {
          label: "API",
          items: [
            { label: "Core", slug: "api/core" },
            { label: "Audio DSP", slug: "api/audio_dsp" },
          ],
        },
      ],
    }),
  ],
});
