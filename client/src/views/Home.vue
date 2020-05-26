<template>
  <b-container class="home">
    <b-row>
      <b-col sm>
        <div v-for="room in rooms" v-bind:key="room.id">
          <a v-on:click.stop="selectRoom(room)" href="#">
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
  rooms: Entity<Room>[] = [];
  activeRoom: Entity<Room> | null = null;

  mounted() {
    ApiService.rooms(true)
      .then(rooms => {
        this.rooms = rooms;
      })
      .catch(console.log);
  }

  selectRoom(room: Entity<Room>) {
    this.activeRoom = room;
  }

  joinRoom(room: Entity<Room>) {
    ApiService.joinRoom(room.id).then(updated => {
      room.data = updated;
      window.open(room.data.zoomLink, "_blank");
    });
  }
}
</script>
