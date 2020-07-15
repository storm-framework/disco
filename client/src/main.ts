// Polyfills
import "core-js/stable";
import "mutationobserver-shim";
// Introspect types
import "reflect-metadata";
import "regenerator-runtime/runtime";
// Vue setup
import Vue from "vue";
import App from "./App.vue";
import IconButton from "./components/IconButton.vue";
import "./plugins/bootstrap-vue";
import "./plugins/vue-fontawesome";

Vue.config.productionTip = false;
Vue.component("icon-button", IconButton);

new App().$mount("#app");
