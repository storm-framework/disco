<template>
  <main v-if="sessionUser" tag="main">
    <h1 class="sr-only">Overview</h1>
    <h2 class="sr-only" v-if="currentRoom">Current Call</h2>
    <jitsi-call
      v-if="currentRoom"
      class="call"
      :room="currentRoom"
      :user="sessionUser"
    />
    <h2 class="sr-only">Your status</h2>
    <section v-if="sessionUser" class="container row">
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
          icon="external-link-alt"
          variant="primary"
          @click="joinRandomRoom"
          target="_blank"
        >
          Random room
        </icon-button>
      </b-col>
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

  joinRandomRoom() {
    const random = _.sample(this.availableRooms);
    if (random) {
      window.open(random.zoomLink, "_blank");
      this.$store.dispatch("joinRoom", random.id);
    }
  }
}
</script>

<style lang="scss" scoped>
.call {
  width: calc(100% - 3rem);
  height: calc(100vh - 3rem);
  margin: 0 auto 1.5rem auto;
}
</style>
