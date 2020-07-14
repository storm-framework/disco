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
          <b-dropdown-item-button @click="messageModal.display = true">
            Send Message
          </b-dropdown-item-button>
        </b-nav-item-dropdown>
        <b-nav-item v-if="showSignOut" @click="signOut">Sign out</b-nav-item>
      </b-navbar-nav>
    </b-navbar>

    <b-modal
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
    </b-modal>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { mapGetters } from "vuex";
import ApiService from "@/services/api";
import { User } from "@/models";

@Component({
  computed: mapGetters(["loggedIn", "currentRoom", "currentVideoRoom"])
})
export default class Navbar extends Vue {
  // The navBar is in charge of syncing the session user with the backend.
  // Other components rely on this.
  messageModal = {
    display: false,
    message: ""
  };

  mounted() {
    this.$store.dispatch("syncSessionUser").catch(error => {
      // Don't panic if the user is not yet authenticated
      if (error?.response?.status !== 401) {
        throw error;
      }
    });
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

  sendMessage() {
    const user: User = this.$store.getters.sessionUser;
    if (user) {
      ApiService.sendMessage({
        senderId: user.id,
        receiverId: null,
        messageText: this.messageModal.message,
        timestamp: new Date().getTime()
      }).then(value => this.clearMessage());
    }
  }

  clearMessage() {
    this.messageModal.message = "";
  }
}
</script>
