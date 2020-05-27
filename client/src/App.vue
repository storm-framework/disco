<template>
  <div id="app">
    <router-view />
  </div>
</template>

<style lang="scss">
// :root {
//   --font-family-sans-serif: -apple-system, BlinkMacSystemFont, "Segoe UI",
//     Roboto, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji",
//     "Segoe UI Emoji", "Segoe UI Symbol";
// }
html,
body {
  height: 100%;
}

body {
  background-color: #f5f5f5;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
    "Helvetica Neue", Arial, "Noto Sans", sans-serif, "Apple Color Emoji",
    "Segoe UI Emoji", "Segoe UI Symbol" !important;
  // font: 1rem/1.5 var(--font-family-sans-serif) !important;
}

#app {
  width: 100%;
  height: 100%;
}

.form-control {
  position: relative;
  box-sizing: border-box;
  height: auto;
  padding: 10px;
  font-size: 16px;
}
</style>
