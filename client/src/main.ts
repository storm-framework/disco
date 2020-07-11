// Polyfills
import "core-js/stable";
import "regenerator-runtime/runtime";

import "mutationobserver-shim";

// Introspect types
import "reflect-metadata";

// Vue setup
import Vue from "vue";
import App from "./App.vue";
import IconButton from "./components/IconButton.vue";
import "./plugins/bootstrap-vue";
import "./plugins/vue-fontawesome";


import BootstrapVue from 'bootstrap-vue'
// import store from './store/store'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'
// import Modal from 'bootstrap-vue/es/components/modal'


Vue.use(BootstrapVue);
// Vue.use(Modal)


Vue.config.productionTip = false;
Vue.component("icon-button", IconButton);

new App().$mount("#app");
