<template>
  <b-navbar toggleable="lg" type="dark" variant="info">
    <b-navbar-brand to="/">Community Video</b-navbar-brand>
    <b-navbar-nav class="ml-auto">
      <b-nav-item-dropdown text="Admin" right v-if="isOrganizer">
        <b-dropdown-item :to="{ name: 'Admin' }">Rooms</b-dropdown-item>
        <b-dropdown-item :to="{ name: 'Invitations' }">
          Invitations
        </b-dropdown-item>
        <b-dropdown-item :to="{ name: 'SendInvitations' }">
          SendInvitations
        </b-dropdown-item>
      </b-nav-item-dropdown>
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
