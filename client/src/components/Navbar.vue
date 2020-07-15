<template>
  <div>
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
          <!-- <b-dropdown-item-button @click="messageModal.display = true"> -->
          <b-dropdown-item-button v-b-modal="'broadcast-modal'">
            Make Announcement
          </b-dropdown-item-button>
        </b-nav-item-dropdown>
        <b-nav-item v-if="showSignOut" @click="signOut">Sign out</b-nav-item>
      </b-navbar-nav>
    </b-navbar>

    <send-message modalId="broadcast-modal" modalTitle="Announce Message" />

    <!-- <b-modal
      id="sendMessage"
      v-model="messageModal.display"
      title="Broadcast Message"
      :header-bg-variant="'info'"
      :header-text-variant="'light'"
      :body-bg-variant="'light'"
      :body-text-variant="'dark'"
      :footer-bg-variant="'dark'"
      :footer-text-variant="'light'"
      :ok-title="'Send'"
      @ok="sendMessage"
      @cancel="clearMessage"
      @keydown.native.enter="sendMessage"
      hide-header-close
      no-close-on-esc
      no-close-on-backdrop
    >
      <form>
        <div>
          <b-form-input
            v-model="messageModal.message"
            placeholder="Hi everyone!"
          ></b-form-input>
        </div>
      </form>
    </b-modal> -->
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { mapGetters } from "vuex";
// import { MessageModal } from "@/models";
import SendMessage from "@/components/SendMessage.vue";

@Component({
  computed: mapGetters(["loggedIn", "currentRoom", "currentVideoRoom"]),
  components: { SendMessage }
})
export default class Navbar extends Vue {
  // The navBar is in charge of syncing the session user with the backend.
  // Other components rely on this.

  // messageModal: MessageModal = {
  //   display: false,
  //   message: "",
  //   receiverId: null
  // };

  mounted() {
    this.$store.dispatch("syncSessionUser");
  }

  get isOrganizer() {
    return this.$store.getters.sessionUser?.level == "organizer";
  }

  get showSignOut() {
    const current = this.$store.getters.currentRoom;
    const video = current && this.$store.getters.isVideoRoom(current);
    return this.$store.getters.loggedIn && !video;
  }

  signOut() {
    this.$store
      .dispatch("signOut")
      .then(() => this.$router.push({ name: "SignIn" }));
  }

}
</script>
