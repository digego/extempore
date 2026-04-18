import { defineConfig } from 'vitepress'

export default defineConfig({
  title: 'Extempore',
  description: 'Multimedia livecoding environment',
  base: '/',
  search: { provider: 'local' },
  markdown: {
    typographer: true,
    languageAlias: {
      'xtlang': 'scheme',
      'extempore': 'scheme'
    }
  },
  themeConfig: {
    nav: [
      { text: 'Docs', link: '/overview/quickstart' },
      { text: 'GitHub', link: 'https://github.com/extemporelang/extempore' }
    ],
    sidebar: [
      {
        text: 'Overview',
        items: [
          { text: 'Quickstart', link: '/overview/quickstart' },
          { text: 'Using Extempore', link: '/overview/using-extempore' },
          { text: 'Philosophy', link: '/overview/philosophy' },
          { text: 'Time', link: '/overview/time' },
          { text: 'Contributing', link: '/overview/contributing' }
        ]
      },
      {
        text: 'Reference',
        items: [
          { text: 'xtlang types', link: '/reference/types' },
          { text: 'Type inferencing', link: '/reference/type-inference' },
          { text: 'Memory management', link: '/reference/memory-management' },
          { text: 'Scheme-xtlang interop', link: '/reference/scheme-xtlang-interop' },
          { text: 'Concurrency', link: '/reference/concurrency' },
          { text: 'C-xtlang interop', link: '/reference/c-xtlang-interop' },
          { text: 'Docstrings', link: '/reference/docstrings' },
          { text: 'Testing', link: '/reference/testing' }
        ]
      },
      {
        text: 'Guides',
        items: [
          { text: 'Editor support', link: '/guides/editor-support' },
          { text: 'Pattern language', link: '/guides/pattern-language' },
          { text: 'Analogue synth', link: '/guides/analogue-synth' },
          { text: 'Audio signal processing', link: '/guides/audio-signal-processing' },
          { text: 'Audio file I/O', link: '/guides/audio-file-io' },
          { text: 'Making an instrument', link: '/guides/making-an-instrument' },
          { text: 'Note-level music', link: '/guides/note-level-music' },
          { text: 'Sampler', link: '/guides/sampler' },
          { text: 'Common Lisp Music', link: '/guides/common-lisp-music' },
          { text: 'Impromptu users', link: '/guides/impromptu-users' }
        ]
      },
      {
        text: 'API',
        items: [
          { text: 'Core', link: '/api/core' },
          { text: 'Audio DSP', link: '/api/audio_dsp' }
        ]
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/extemporelang/extempore' }
    ],
    editLink: {
      pattern: 'https://github.com/extemporelang/extempore/edit/master/docs/:path'
    }
  }
})
