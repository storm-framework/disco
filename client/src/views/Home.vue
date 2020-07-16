<template>
  <main v-if="sessionUser" tag="main">
    <h1 class="sr-only">Overview</h1>
    <h2 class="sr-only" v-if="inRoom">Current Call</h2>
    <jitsi-call
      class="call"
      v-if="inRoom"
      :room="currentRoom"
      :user="sessionUser"
      @joined="joinRoom"
      @left="leaveRoom"
    />
    <h2 class="sr-only">Your status</h2>
    <section v-if="sessionUser" class="container mt-4">
      <b-row>
        <user-summary
          long
          editable
          v-bind="sessionUser"
          :h-context="3"
          class="col-6"
        />
        <room-card
          v-if="inRoom"
          :room="currentRoom"
          :h-context="4"
          class="col-6"
        />
        <lobby v-else :h-context="4" class="col-6" />
      </b-row>
    </section>
    <section v-if="roomsAreAvailable" class="container">
      <h2 v-if="currentRoom" class="mt-5">Other Rooms</h2>
      <h2 v-else class="mt-5">All Rooms</h2>
      <ul class="row list-unstyled mt-4">
        <li class="mb-4 col-lg-6" v-for="room in availableRooms" :key="room.id">
          <room-card :room="room" :h-context="3" class="available-room" />
        </li>
      </ul>
    </section>
  </main>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import RoomCard from "@/components/RoomCard.vue";
import Lobby from "@/components/Lobby.vue";
import UserSummary from "@/components/UserSummary.vue";
import JitsiCall from "@/components/JitsiCall.vue";
import { Room, RecvMessage, User } from "@/models";
import { mapGetters } from "vuex";
import { faDice } from "@fortawesome/free-solid-svg-icons";
import { library } from "@fortawesome/fontawesome-svg-core";
import { BvEvent } from "bootstrap-vue";
library.add(faDice);

const SYNC_INTERVAL = 10000;

function timeStampString(timeStamp: number) {
  const date = new Date(timeStamp);
  return date.toString().split(" GMT")[0];
}

@Component({
  components: { RoomCard, UserSummary, JitsiCall, Lobby },
  computed: mapGetters(["sessionUser", "currentRoom"])
})
export default class Home extends Vue {
  syncing = false;
  syncCount = 0;
  msgSyncing = false;
  msgQueue: RecvMessage[] = [];
  currentRoom!: Room;
  syncTimerHandler: number | null = null;

  get inRoom() {
    return this.$store.getters.currentRoom !== null;
  }

  get roomsAreAvailable() {
    return this.availableRooms.length !== 0;
  }

  get availableRooms(): Room[] {
    return this.$store.getters.availableRooms;
  }

  mounted() {
    this.$root.$on("bv::toast:hide", (event: BvEvent) => {
      const m = /MESSAGE::(\d+)/.exec(event.componentId || "");
      if (m && m[1]) {
        this.$store.dispatch("markAsRead", parseInt(m[1]));
      }
    });
    this.sync();
  }

  beforeDestroy() {
    if (this.syncTimerHandler) {
      clearTimeout(this.syncTimerHandler);
    }
  }

  sync() {
    if (this.syncing) {
      return;
    }
    this.syncing = true;
    this.$store.dispatch("sync").then((messages: RecvMessage[]) => {
      this.syncing = false;
      for (const m of messages) {
        this.showMessage(m);
      }
      this.syncTimerHandler = setTimeout(this.sync, SYNC_INTERVAL);
    });
  }

  showMessage(message: RecvMessage) {
    const e = this.$createElement;
    const sender: User | null = this.$store.getters.userById(message.senderId);
    this.$bvToast.toast(
      e("div", {}, [
        e("i", {}, timeStampString(message.timestamp)),
        e("div", {}, message.messageText)
      ]),
      {
        id: `MESSAGE::${message.messageId}`,
        title: `${sender?.displayName} says ...`,
        solid: true,
        noAutoHide: true,
        appendToast: true,
        variant: "secondary"
      }
    );
  }

  joinRoom(roomId: number) {
    this.$store.dispatch("joinRoom", roomId).catch(error => {
      if (error.response?.status == 409) {
        this.showError("Sorry, but the room is already full");
      } else {
        this.showError("An unexpected error happend");
      }
    });
  }

  leaveRoom() {
    this.$store.dispatch("leaveRoom");
  }

  showError(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Error",
      toaster: "b-toaster-top-center",
      variant: "danger",
      solid: true
    });
  }
}
</script>

<style lang="scss" scoped>
.call {
  // Copied from youtube's theater mode
  width: 100%;
  height: calc((9 / 16) * 100vw);
  max-height: calc(100vh - 169px);
  min-height: 480px;
}
</style>
