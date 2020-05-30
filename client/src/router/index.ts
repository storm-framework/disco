import ApiService from "@/services/api";
import Admin from "@/views/Admin.vue";
import Home from "@/views/Home.vue";
import Invitations from "@/views/Invitations.vue";
import SendInvitations from "@/views/SendInvitations.vue";
import SignIn from "@/views/SignIn.vue";
import SignUp from "@/views/SignUp.vue";
import _ from "lodash";
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
    path: "/invitation",
    name: "Invitations",
    component: Invitations
  },
  {
    path: "/invitation/send",
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
  }
];

const router = new VueRouter({
  mode: "history",
  base: process.env.BASE_URL,
  routes
});

router.beforeEach((to, from, next) => {
  if (!_.includes(["SignIn", "SignUp"], to.name) && !ApiService.signedIn()) {
    next({ name: "SignIn" });
  } else {
    next();
  }
});

export default router;
