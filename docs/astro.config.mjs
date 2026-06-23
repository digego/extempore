import { defineConfig } from 'astro/config'
import starlight from '@astrojs/starlight'

// Honour `## Heading {#custom-id}` syntax (supported by the old VitePress site,
// not by Astro out of the box): strip the marker and set the heading's id.
function remarkHeadingIds() {
  const walk = (node) => {
    if (node.type === 'heading' && node.children?.length) {
      const last = node.children[node.children.length - 1]
      if (last.type === 'text') {
        const m = last.value.match(/\s*\{#([\w-]+)\}\s*$/)
        if (m) {
          last.value = last.value.slice(0, last.value.length - m[0].length)
          node.data = node.data || {}
          node.data.hProperties = { ...(node.data.hProperties || {}), id: m[1] }
          node.data.id = m[1]
        }
      }
    }
    if (node.children) node.children.forEach(walk)
  }
  return (tree) => walk(tree)
}

// https://astro.build/config
export default defineConfig({
  markdown: {
    remarkPlugins: [remarkHeadingIds],
  },
  site: 'https://extemporelang.github.io',
  base: '/',
  // type-inference was folded into the types reference page; keep the old URL working.
  redirects: {
    '/reference/type-inference/': '/reference/types/#type-inferencing',
  },
  integrations: [
    starlight({
      title: 'Extempore',
      description: 'Livecoding environment for music and audio',
      social: [
        {
          icon: 'github',
          label: 'GitHub',
          href: 'https://github.com/digego/extempore',
        },
      ],
      editLink: {
        baseUrl: 'https://github.com/digego/extempore/edit/master/docs/',
      },
      // xtlang and extempore code fences are highlighted as Scheme.
      expressiveCode: {
        shiki: {
          langAlias: {
            xtlang: 'scheme',
            extempore: 'scheme',
          },
        },
      },
      sidebar: [
        {
          label: 'Start here',
          items: [
            { label: 'Quickstart', slug: 'overview/quickstart' },
            { label: 'Using Extempore', slug: 'overview/using-extempore' },
            { label: 'Philosophy', slug: 'overview/philosophy' },
            { label: 'Contributing', slug: 'overview/contributing' },
          ],
        },
        {
          label: 'Learn xtlang',
          items: [
            { label: 'xtlang tutorial', slug: 'reference/tutorial' },
            { label: 'Time and scheduling', slug: 'overview/time' },
          ],
        },
        {
          label: 'xtlang reference',
          items: [
            { label: 'Overview', slug: 'reference' },
            { label: 'Types', slug: 'reference/types' },
            { label: 'Memory management', slug: 'reference/memory-management' },
            { label: 'Concurrency', slug: 'reference/concurrency' },
            { label: 'Scheme-xtlang interop', slug: 'reference/scheme-xtlang-interop' },
            { label: 'C-xtlang interop', slug: 'reference/c-xtlang-interop' },
            { label: 'Error messages', slug: 'reference/error-messages' },
            { label: 'Docstrings', slug: 'reference/docstrings' },
            { label: 'Testing', slug: 'reference/testing' },
          ],
        },
        {
          label: 'Guides',
          items: [
            { label: 'Overview', slug: 'guides' },
            { label: 'Editor support', slug: 'guides/editor-support' },
            { label: 'Pattern language', slug: 'guides/pattern-language' },
            { label: 'Analogue synth', slug: 'guides/analogue-synth' },
            { label: 'Audio signal processing', slug: 'guides/audio-signal-processing' },
            { label: 'Audio file I/O', slug: 'guides/audio-file-io' },
            { label: 'Making an instrument', slug: 'guides/making-an-instrument' },
            { label: 'Note-level music', slug: 'guides/note-level-music' },
            { label: 'Sampler', slug: 'guides/sampler' },
          ],
        },
        {
          label: 'API',
          items: [
            { label: 'Core', slug: 'api/core' },
            { label: 'Audio DSP', slug: 'api/audio_dsp' },
          ],
        },
      ],
    }),
  ],
})
