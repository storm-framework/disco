<template>
  <main v-if="sessionUser" tag="main">
    <b-modal
      id="showMessage"
      title="Message"
      :header-bg-variant="'info'"
      :header-text-variant="'light'"
      :body-bg-variant="'light'"
      :body-text-variant="'dark'"
      :footer-bg-variant="'dark'"
      :footer-text-variant="'light'"
      :ok-title="'Ok'"
      @ok="receivedMessage"
      ok-only
      hide-header-close
      no-close-on-esc
      no-close-on-backdrop
    >
      <template slot="modal-title">
        <h3>{{ messageModal.sender }} says ...</h3>
      </template>
      <p class="font-italic">At {{ messageModal.time }}</p>
      <p>{{ messageModal.message }}</p>
    </b-modal>

    <h1 class="sr-only">Overview</h1>
    <h2 class="sr-only" v-if="currentVideoRoom">Current Call</h2>
    <transition name="appear">
      <jitsi-call
        v-if="currentVideoRoom"
        class="call"
        :room="currentRoom"
        :user="sessionUser"
        @joined="joinRoom"
        @left="leaveRoom"
      />
    </transition>
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
          v-if="currentRoom"
          :room="currentRoom"
          :h-context="4"
          class="col-6"
        />
        <b-col
          v-else
          cols="6"
          class="align-items-center d-flex flex-column justify-content-center"
        >
          <p class="h5">
            You are invisible to others until you join a room
          </p>
          <b-dropdown
            variant="primary"
            id="dropdown-1"
            text="Join"
            class="m-md-2"
          >
            <b-dropdown-item
              v-if="roomsAreAvailable"
              icon="dice"
              @click="joinRandomRoom"
            >
              Random room
            </b-dropdown-item>
            <b-dropdown-item
              v-if="waitingRooms.length > 0"
              icon="dice"
              @click="joinWaitingRoom"
            >
              Waiting area
            </b-dropdown-item>
          </b-dropdown>
          <!-- <div>
            <icon-button
              v-if="roomsAreAvailable"
              icon="dice"
              variant="primary"
              @click="joinRandomRoom"
            >
              Random room
            </icon-button>

            <icon-button
              v-if="waitingRooms.length > 0"
              icon="dice"
              variant="success"
              @click="joinWaitingRoom"
            >
              Waiting room
            </icon-button>
          </div> -->
        </b-col>
      </b-row>
    </section>
    <section v-if="roomsAreAvailable" class="container">
      <h2 v-if="currentRoom" class="mt-5">Other Rooms</h2>
      <h2 v-else class="mt-5">All Rooms</h2>
      <ul class="row list-unstyled mt-4">
        <li
          class="mb-4 col-lg-auto"
          v-for="room in availableRooms"
          :key="room.id"
        >
          <room-card :room="room" :h-context="3" class="available-room" />
        </li>
      </ul>
    </section>
  </main>
</template>

<script lang="ts">
import { Component, Vue, Watch } from "vue-property-decorator";
import RoomCard from "@/components/RoomCard.vue";
import UserSummary from "@/components/UserSummary.vue";
import JitsiCall from "@/components/JitsiCall.vue";
import { Room, RecvMessage } from "@/models";
import { mapGetters } from "vuex";
import _ from "lodash";
import { faDice } from "@fortawesome/free-solid-svg-icons";
import { library } from "@fortawesome/fontawesome-svg-core";
library.add(faDice);

const SYNC_INTERVAL = 10000;
const MSG_SYNC_INTERVAL = 1000;

function timeStampString(timeStamp: number) {
  const date = new Date(timeStamp);
  return date.toString().split(" GMT")[0];
}

@Component({
  components: { RoomCard, UserSummary, JitsiCall },
  computed: mapGetters(["sessionUser", "currentRoom", "waitingRooms"])
})
export default class Home extends Vue {
  syncing = false;
  syncCount = 0;
  msgSyncing = false;
  msgQueue: RecvMessage[] = [];
  currentRoom!: Room;
  syncTimerHandler: number | null = null;
  msgSyncTimerHandler: number | null = null;

  messageModal = {
    display: false,
    sender: "",
    time: "",
    message: "",
    messageId: -1
  };

  get currentVideoRoom() {
    const current = this.$store.getters.currentRoom;
    return current && this.$store.getters.isVideoRoom(current);
  }

  get roomsAreAvailable() {
    return this.availableRooms.length !== 0;
  }

  get emptyRoomsAreAvailable() {
    return this.emptyRooms.length !== 0;
  }

  get availableRooms(): Room[] {
    return this.$store.getters.availableRooms;
  }

  get emptyRooms(): Room[] {
    return this.$store.getters.emptyRooms;
  }

  mounted() {
    this.sync();
    this.msgSync();
  }

  beforeDestroy() {
    console.log("ABOUT TO DESTROY");
    if (this.syncTimerHandler) {
      clearTimeout(this.syncTimerHandler);
    }
    if (this.msgSyncTimerHandler) {
      clearTimeout(this.msgSyncTimerHandler);
    }
  }

  // trigger message alerts when msgQueue changes, by popping first message
  // and displaying it as a modal: the handler for the 'ok' will recursively
  // retrigger to drain more messages.
  @Watch("msgQueue")
  showMessages() {
    // console.log("showMessages", this.msgQueue);
    const paused = this.$store.getters.isPauseAlert;

    if (paused) {
      // console.log("is-paused-alert");
      return;
    }
    if (this.msgQueue.length == 0) {
      // console.log("is-no-more-messages");
      return;
    }
    // pop first msg
    const msg = this.msgQueue[0];
    this.msgQueue.shift();

    // display it
    this.showMessage(msg);
  }

  showMessage(msg: RecvMessage) {
    const isRead = this.$store.getters.isRead(msg.messageId);
    if (isRead) {
      this.showMessages();
    } else {
      this.$store.dispatch("pauseAlerts").then(() => {
        this.messageModal.sender = this.$store.getters.userById(
          msg.senderId
        ).displayName;
        this.messageModal.time = timeStampString(msg.timestamp);
        this.messageModal.message = msg.messageText;
        this.messageModal.messageId = msg.messageId;
        this.$bvModal.show("showMessage");
      });
    }
  }

  receivedMessage() {
    this.$bvModal.hide("showMessage");
    this.$store.dispatch("markRead", this.messageModal.messageId).then(() => {
      // console.log("resume-messages");
      this.showMessages();
    });
  }

  sync() {
    if (this.syncing) {
      return;
    }
    this.syncing = true;
    this.$store.dispatch("sync").then(() => {
      this.syncing = false;
      this.syncTimerHandler = setTimeout(this.sync, SYNC_INTERVAL);
    });
    // this.getMessages();
  }

  // update msgQueue with messages
  // getMessages() {
  //   this.$store.dispatch("recvMessages").then(msgs => {
  //     if (msgs.length > 0) {
  //       // console.log("getMessages", msgs);
  //       this.msgQueue = msgs;
  //     }
  //   });
  // }

  msgSync() {
    if (this.msgSyncing) {
      return;
    }
    this.msgSyncing = true;
    this.$store.dispatch("recvMessages").then(msgs => {
      // console.log("getMessages", msgs);
      this.msgSyncing = false;
      this.msgSyncTimerHandler = setTimeout(this.msgSync, MSG_SYNC_INTERVAL);
      if (msgs.length > 0) {
        this.msgQueue = msgs;
      }
    });
  }

  joinRoom(roomId: string) {
    this.$store.dispatch("joinRoom", roomId);
  }

  leaveRoom() {
    this.$store.dispatch("leaveRoom");
  }

  joinRandomRoom() {
    const random = _.sample(this.availableRooms);
    if (random) {
      this.$store.dispatch("joinRoom", random.id);
    }
  }

  joinWaitingRoom() {
    const random = _.sample(this.$store.getters.waitingRooms);
    if (random) {
      this.$store.dispatch("joinRoom", random.id);
    }
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
