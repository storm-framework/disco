<template>
  <b-navbar toggleable="lg" type="dark" variant="info">
    <b-navbar-brand to="/">Community Video</b-navbar-brand>
    <b-navbar-nav class="ml-auto">
      <b-nav-item v-if="isOrganizer" :to="{ name: 'Admin' }">
        Admin
      </b-nav-item>
      <b-nav-item v-if="loggedIn" @click="signOut">Sign out</b-nav-item>
    </b-navbar-nav>
  </b-navbar>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { mapGetters } from "vuex";

@Component({
  computed: mapGetters(["loggedIn"])
})
export default class Navbar extends Vue {
  get isOrganizer() {
    // This is not ideal because it only works if we have the logged in user,
    // but doing a whole sync just to find out if the current user is an admin
    // seems overkill
    return this.$store.getters.sessionUser?.level == "organizer";
  }

  signOut() {
    this.$store
      .dispatch("signOut")
      .then(() => this.$router.push({ name: "SignIn" }));
  }
}
</script>
