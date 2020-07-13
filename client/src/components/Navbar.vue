<template>
  <b-navbar toggleable="lg" type="dark" variant="primary">
    <b-navbar-brand to="/">Distant Socialing</b-navbar-brand>
    <b-navbar-nav class="ml-auto">
      <b-nav-item-dropdown text="Admin" right v-if="isOrganizer">
        <b-dropdown-item :to="{ name: 'Admin' }">Rooms</b-dropdown-item>
        <b-dropdown-item :to="{ name: 'Invitations' }">
          Invitations
        </b-dropdown-item>
        <b-dropdown-item :to="{ name: 'SendInvitations' }">
          Send Invitations
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
  // The navBar is in charge of syncing the session user with the backend.
  // Other components rely on this.
  mounted() {
    this.$store.dispatch("syncSessionUser");
  }

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
