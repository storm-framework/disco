<template>
  <b-container v-if="sessionUser" tag="main">
    <h1 class="sr-only">Overview</h1>
    <h2 class="sr-only">Your status</h2>
    <section v-if="sessionUser" class="row">
      <b-media
        tag="section"
        vertical-align="center"
        class="col-7 align-items-center"
      >
        <template v-slot:aside>
          <b-avatar :src="sessionUser.photoURL" :text="initials" size="300" />
        </template>

        <h3>{{ fullName }}</h3>

        <dl>
          <dt>Badge name</dt>
          <dd>{{ sessionUser.displayName }}</dd>
        </dl>
      </b-media>

      <room-card
        v-if="currentRoom"
        :room="currentRoom"
        :h-context="4"
        class="col-5"
      />
      <p v-else class="h5 align-self-center ml-5">You have not joined a room</p>
    </section>
    <section v-if="roomsAreAvailable">
      <h2 v-if="currentRoom" class="mt-5">Other Rooms</h2>
      <h2 v-else class="mt-5">All Rooms</h2>
      <ul class="row list-unstyled mt-4">
        <li class="mb-4 col-4" v-for="room in availableRooms" :key="room.id">
          <room-card :room="room" :h-context="3" class="available-room" />
        </li>
      </ul>
    </section>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import RoomCard from "@/components/RoomCard.vue";
import { Room } from "@/models";
import { mapGetters } from "vuex";
import _ from "lodash";

const SYNC_INTERVAL = 10000;

@Component({
  components: { RoomCard },
  computed: mapGetters(["sessionUser", "currentRoom"])
})
export default class Home extends Vue {
  syncing = false;
  currentRoom!: Room;
  syncTimerHandler: number | null = null;

  get roomsAreAvailable() {
    return this.availableRooms.length !== 0;
  }

  get fullName() {
    const sessionUser = this.$store.getters.sessionUser;
    return _.trim(_.join([sessionUser?.firstName, sessionUser?.lastName], " "));
  }

  get initials() {
    const sessionUser = this.$store.getters.sessionUser;
    const fn = sessionUser?.firstName[0] || "";
    const ln = sessionUser?.lastName[0] || "";
    return _.trim(fn + ln);
  }

  get availableRooms() {
    const allRooms = this.$store.getters.rooms;
    if (this.currentRoom) {
      return _.filter(allRooms, room => room.id !== this.currentRoom.id);
    } else {
      return allRooms;
    }
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
}
</script>

<style lang="scss" scoped>
.available-room {
  height: 100%;
}
</style>
