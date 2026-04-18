import DefaultTheme from 'vitepress/theme'
import './style.css'
import BetaBanner from './BetaBanner.vue'
import { h } from 'vue'

export default {
  extends: DefaultTheme,
  Layout() {
    return h(DefaultTheme.Layout, null, {
      'home-hero-info-before': () => h(BetaBanner)
    })
  }
}
