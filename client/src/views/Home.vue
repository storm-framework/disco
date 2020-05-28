<template>
  <div class="home">
    <navbar :activity="syncing" />
    <div v-if="sessionUser">
      <b-container>
        <h2 class="mt-3">{{ sessionUser.fullName }}</h2>
        <div v-if="currentRoom">
          You are currently in
          <a v-on:click.prevent.stop="selectRoom(currentRoom.id)" href="#">
            {{ currentRoom.name }}
          </a>
        </div>
      </b-container>
      <hr />
      <b-container>
        <b-row class="mt-5">
          <b-col sm>
            <h3>Rooms</h3>
            <div v-for="room in rooms" v-bind:key="room.id">
              <a v-on:click.prevent.stop="selectRoom(room.id)" href="#">
                {{ room.name }}
              </a>
            </div>
          </b-col>

          <b-col sm>
            <div class="active-room" v-if="activeRoom">
              <h3>
                {{ activeRoom.name }}
              </h3>
              <b-overlay
                :show="joiningRoom"
                class="d-inline-block"
                spinner-small
                round
              >
                <b-button v-on:click="joinRoom(activeRoom)">
                  Join Room
                </b-button>
              </b-overlay>
              <h4 class="mt-5">Users</h4>
              <div v-for="user in activeRoom.users" v-bind:key="user.id">
                {{ user.displayName }}
              </div>
            </div>
          </b-col>
        </b-row>
      </b-container>
    </div>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import Navbar from "@/components/Navbar.vue";
import { mapGetters } from "vuex";

const SYNC_INTERVAL = 10000;

@Component({
  components: { Navbar },
  computed: mapGetters(["activeRoom", "rooms", "sessionUser", "currentRoom"])
})
export default class Home extends Vue {
  joiningRoom = false;
  syncing = false;

  mounted() {
    this.sync();
  }

  sync() {
    if (this.syncing) {
      return;
    }
    this.syncing = true;
    this.$store.dispatch("sync").then(() => {
      this.syncing = false;
      setTimeout(this.sync, SYNC_INTERVAL);
    });
  }

  selectRoom(roomId: string) {
    this.$store.dispatch("selectRoom", roomId);
  }

  joinRoom(roomId: string) {
    this.joiningRoom = true;
    this.$store.dispatch("joinRoom", roomId).then(zoomLink => {
      this.joiningRoom = false;
      window.open(zoomLink, "_blank");
    });
  }
}
</script>
