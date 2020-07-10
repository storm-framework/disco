<template>
  <main v-if="sessionUser" tag="main">
    <h1 class="sr-only">Overview</h1>
    <h2 class="sr-only" v-if="currentRoom">Current Call</h2>
    <transition name="appear">
      <jitsi-call
        v-if="currentRoom"
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
          cols="4"
          class="align-items-center d-flex flex-column justify-content-center"
        >
          <p class="h5">
            You have not joined a room
          </p>
          <icon-button
            v-if="roomsAreAvailable"
            icon="dice"
            variant="primary"
            @click="joinRandomRoom"
          >
            Random room
          </icon-button>
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
import { Component, Vue } from "vue-property-decorator";
import RoomCard from "@/components/RoomCard.vue";
import UserSummary from "@/components/UserSummary.vue";
import JitsiCall from "@/components/JitsiCall.vue";
import { Room } from "@/models";
import { mapGetters } from "vuex";
import _ from "lodash";

import { faDice } from "@fortawesome/free-solid-svg-icons";
import { library } from "@fortawesome/fontawesome-svg-core";
library.add(faDice);

const SYNC_INTERVAL = 10000;

@Component({
  components: { RoomCard, UserSummary, JitsiCall },
  computed: mapGetters(["sessionUser", "currentRoom"])
})
export default class Home extends Vue {
  syncing = false;
  currentRoom!: Room;
  syncTimerHandler: number | null = null;

  get roomsAreAvailable() {
    return this.availableRooms.length !== 0;
  }

  get availableRooms(): Room[] {
    return this.$store.getters.availableRooms;
  }

  mounted() {
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
    this.$store.dispatch("sync").then(() => {
      this.syncing = false;
      this.syncTimerHandler = setTimeout(this.sync, SYNC_INTERVAL);
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
