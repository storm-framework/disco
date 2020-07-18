<template>
  <div>
    <b-navbar toggleable="lg" type="dark" variant="primary">
      <b-navbar-brand to="/">Distant Socialing</b-navbar-brand>
      <b-navbar-nav class="ml-auto">
        <b-nav-item-dropdown text="Admin" right v-if="isOrganizer">
          <b-dropdown-item :to="{ name: 'Admin' }" :target="routesTarget">
            Manage Rooms
          </b-dropdown-item>
          <b-dropdown-item :to="{ name: 'Invitations' }" :target="routesTarget">
            See Invitations
          </b-dropdown-item>
          <b-dropdown-item
            :to="{ name: 'SendInvitations' }"
            :target="routesTarget"
          >
            Send Invitations
          </b-dropdown-item>
          <b-dropdown-item-button v-b-modal.broadcast-modal>
            Make Announcement
          </b-dropdown-item-button>
        </b-nav-item-dropdown>
        <b-nav-item-dropdown text="Settings" right v-if="loggedIn">
          <b-dropdown-item-button v-b-modal.edit-profile-modal>
            Edit Profile
          </b-dropdown-item-button>
          <b-dropdown-item-button @click="signOut">
            Sign out
          </b-dropdown-item-button>
        </b-nav-item-dropdown>
      </b-navbar-nav>
    </b-navbar>

    <send-message modalId="broadcast-modal" />
    <modal-profile id="edit-profile-modal" />
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { mapGetters } from "vuex";
import SendMessage from "@/components/SendMessage.vue";
import ModalProfile from "@/components/ModalProfile.vue";

@Component({
  computed: mapGetters(["loggedIn", "currentRoom", "currentVideoRoom"]),
  components: { SendMessage, ModalProfile }
})
export default class Navbar extends Vue {
  // The navBar is in charge of syncing the session user with the backend.
  // Other components rely on this.
  mounted() {
    this.$store.dispatch("syncSessionUser");
  }

  get routesTarget() {
    return this.$router.currentRoute.name === "Home" ? "_blank" : undefined;
  }

  get isOrganizer() {
    return this.$store.getters.sessionUser?.level == "organizer";
  }

  get showSignOut() {
    const current = this.$store.getters.currentRoom;
    return this.$store.getters.loggedIn && current !== null;
  }

  signOut() {
    this.$store
      .dispatch("signOut")
      .then(() => this.$router.push({ name: "SignIn" }));
  }
}
</script>
