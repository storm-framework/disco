import Call from "@/views/Call.vue";
import Home from "@/views/Home.vue";
import SignIn from "@/views/SignIn.vue";
import SignUp from "@/views/SignUp.vue";
import Vue from "vue";
import VueRouter, { RouteConfig } from "vue-router";

Vue.use(VueRouter);

const routes: Array<RouteConfig> = [
  {
    path: "/signin",
    name: "SignIn",
    component: SignIn
  },
  {
    path: "/signup",
    name: "SignUp",
    component: SignUp
  },
  {
    path: "/",
    name: "Home",
    component: Home
  },
  {
    path: "/invitation",
    name: "Invitations",
    component: () =>
      import(/* webpackChunkName: "admin" */ "@/views/Invitations.vue")
  },
  {
    path: "/invitation/send",
    name: "SendInvitations",
    component: () =>
      import(/* webpackChunkName: "admin" */ "@/views/SendInvitations.vue")
  },
  {
    path: "/admin",
    name: "Admin",
    component: () => import(/* webpackChunkName: "admin" */ "@/views/Admin.vue")
  },
  {
    path: "/callTest",
    name: "CallTest",
    component: Call
  }
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes
});

export default router;
