import "@babel/polyfill";
import "@fortawesome/fontawesome-free/css/all.css";
import "handsontable/dist/handsontable.full.css";
import "mutationobserver-shim";
import Vue from "vue";
import VueProgress from "vue-progress-path";
import "vue-progress-path/dist/vue-progress-path.css";
import App from "./App.vue";
import "./plugins/bootstrap-vue";
import router from "./router";
import store from "./store";

Vue.config.productionTip = false;

Vue.use(VueProgress);

new Vue({
  router,
  store,
  render: h => h(App)
}).$mount("#app");
