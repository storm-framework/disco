<template>
  <article :style="{ borderTopColor: color }" class="card">
    <b-card-body>
      <heading :level="1" :context="headingContext" class="card-title h4">
        {{ room.name }}
        <span v-if="isCurrentRoom" class="badge badge-secondary">
          Current Room
        </span>
      </heading>

      <b-card-text v-if="isCurrentRoom">
        You are in this room. If you accidentally left the chat,
        <a :href="room.zoomLink">
          click here to rejoin. <font-awesome-icon icon="external-link-alt" />
        </a>
      </b-card-text>

      <b-card-text v-if="empty">
        The room is empty.
      </b-card-text>
      <template v-else>
        <heading :level="2" :context="headingContext" class="sr-only">
          In this room
        </heading>

        <ul>
          <li v-for="user in users" :key="user.id">
            {{ user.displayName }}
          </li>
        </ul>
      </template>

      <b-button v-if="isCurrentRoom" @click="leaveRoom" variant="danger">
        <font-awesome-icon icon="door-open" class="btn-icon" />
        Leave
      </b-button>
      <b-button
        v-else
        variant="primary"
        @click.passive="joinRoom"
        :href="room.zoomLink"
        target="_blank"
      >
        <font-awesome-icon icon="external-link-alt" class="btn-icon" />
        Join
      </b-button>
    </b-card-body>
  </article>
</template>

<script lang="ts">
import { Component, Prop, Mixins } from "vue-property-decorator";
import { Room } from "@/models";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faDoorOpen,
  faComments,
  faExternalLinkAlt
} from "@fortawesome/free-solid-svg-icons";

library.add(faDoorOpen, faComments, faExternalLinkAlt);

const GOLDEN_RATIO: number = (1 + Math.sqrt(5)) / 2;

@Component({ components: { Heading } })
export default class RoomCard extends Mixins(HeadingContext) {
  @Prop() readonly room!: Room;

  static color(id: number) {
    // Using the golden ratio means that colors get evenly spread out and never overlap.
    const hue: number = id * GOLDEN_RATIO;
    return `hsl(${hue}turn, 85%, 60%)`;
  }

  static hashId(id: string) {
    let hash = 0;
    for (let i = 0; i < id.length; i++) {
      hash = (hash << 5) - hash + id.charCodeAt(i);
      hash |= 0;
    }
    return hash;
  }

  get color() {
    const id = parseInt(this.room.id, 10) || RoomCard.hashId(this.room.id);
    return RoomCard.color(id);
  }

  get empty() {
    return this.users.length === 0;
  }

  get users() {
    return this.$store.getters.roomUsers(this.room.id);
  }

  get isCurrentRoom() {
    return this.$store.getters.currentRoom.id === this.room.id;
  }

  joinRoom() {
    this.$store.dispatch("joinRoom", this.room.id);
  }

  leaveRoom() {
    console.log("unimplemented");
  }
}
</script>

<style lang="scss" scoped>
.card {
  overflow: hidden;
  border-top-width: 1rem;
}

.btn {
  font-weight: bold;
}

.btn-icon {
  height: 1em;
  margin-right: 0.25em;
}

.badge {
  overflow: hidden;
  vertical-align: -0.25em;
}
</style>
