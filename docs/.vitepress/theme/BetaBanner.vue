<template>
  <div class="beta-banner">
    These docs are in beta --- some content may be outdated or incomplete.
    <a href="https://github.com/extemporelang/extempore/issues">Feedback welcome.</a>
  </div>
</template>

<style scoped>
.beta-banner {
  background-color: var(--vp-c-warning-soft);
  border: 1px solid var(--vp-c-warning-1);
  border-radius: 8px;
  padding: 12px 20px;
  margin-bottom: 24px;
  font-size: 0.95em;
  text-align: center;
  color: var(--vp-c-text-1);
}

.beta-banner a {
  color: var(--vp-c-brand-1);
  text-decoration: underline;
}
</style>
