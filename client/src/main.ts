import "@babel/polyfill";
import { BootstrapVueIcons } from "bootstrap-vue";
import "mutationobserver-shim";
import Vue from "vue";
import VueProgress from "vue-progress-path";
import "vue-progress-path/dist/vue-progress-path.css";
import App from "./App.vue";
import "./plugins/bootstrap-vue";
import router from "./router";
import store from "./store";

Vue.config.productionTip = false;

Vue.use(BootstrapVueIcons);
Vue.use(VueProgress);

new Vue({
  router,
  store,
  render: h => h(App)
}).$mount("#app");
