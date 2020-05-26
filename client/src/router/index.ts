import Vue from "vue";
import VueRouter, { RouteConfig } from "vue-router";
import Home from "@/views/Home.vue";
import SignIn from "@/views/SignIn.vue";
import SignUp from "@/views/SignUp.vue";
import SendInvitations from "@/views/SendInvitations.vue";
import Admin from "@/views/Admin.vue";

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
    path: "/invitations",
    name: "SendInvitations",
    component: SendInvitations
  },
  {
    path: "/admin",
    name: "Admin",
    component: Admin
  },
  {
    path: "/",
    name: "Home",
    component: Home
  },
  {
    path: "/about",
    name: "About",
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () =>
      import(/* webpackChunkName: "about" */ "../views/About.vue")
  }
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes
});

export default router;
