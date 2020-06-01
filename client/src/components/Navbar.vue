<template>
  <b-navbar toggleable="lg" type="dark" variant="info">
    <b-navbar-brand to="/">Community Video</b-navbar-brand>
    <b-navbar-nav class="ml-auto">
      <b-nav-item v-if="isOrganizer" :to="{ name: 'Admin' }">
        Admin
      </b-nav-item>
      <b-nav-item @click="signOut">Sign out</b-nav-item>
    </b-navbar-nav>
  </b-navbar>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class Navbar extends Vue {
  get isOrganizer() {
    return this.$store.getters.sessionUser?.level == "organizer";
  }

  signOut() {
    this.$store
      .dispatch("signOut")
      .then(() => this.$router.push({ name: "SignIn" }));
  }
}
</script>
