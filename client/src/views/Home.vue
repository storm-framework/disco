<template>
  <b-container class="home">
    <b-row>
      <b-col sm>
        <div v-for="room in rooms" v-bind:key="room.id">
          <a v-on:click.stop="selectRoom(room.id)" href="#">
            {{ room.data.name }}
          </a>
        </div>
      </b-col>

      <b-col sm>
        <div class="active-room" v-if="activeRoom">
          {{ activeRoom.data.name }}
          <a v-on:click.stop="joinRoom(activeRoom)" href="#">
            Join Room
          </a>
          <div v-for="user in activeRoom.data.users" v-bind:key="user.id">
            {{ user.displayName }}
          </div>
        </div>
      </b-col>
    </b-row>
  </b-container>
</template>

<script lang="ts">
// @ is an alias to /src
import { Component, Vue } from "vue-property-decorator";
import ApiService from "@/services/api";
import { Room, Entity } from "@/models";

@Component
export default class Home extends Vue {
  get activeRoom(): Entity<Room> | null {
    return this.$store.getters.activeRoom;
  }

  get rooms(): Entity<Room>[] {
    return this.$store.getters.roomsWithUsers;
  }

  mounted() {
    this.$store.dispatch("initHome");
  }

  selectRoom(roomId: string) {
    this.$store.dispatch("selectRoom", roomId);
  }

  joinRoom(room: Entity<Room>) {
    ApiService.joinRoom(room.id).then(zoomLink => {
      window.open(zoomLink, "_blank");
    });
  }
}
</script>
