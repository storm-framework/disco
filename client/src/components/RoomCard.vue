<template>
  <article :style="{ borderTopColor: room.color }" class="card">
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

        <ul class="user-list">
          <li v-for="user in users" :key="user.id">
            <span v-b-tooltip.hover.html="userTip(user)">
              {{ user.displayName }}
            </span>
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
import { Room, User } from "@/models";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";
import _ from "lodash";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faDoorOpen,
  faComments,
  faExternalLinkAlt
} from "@fortawesome/free-solid-svg-icons";

library.add(faDoorOpen, faComments, faExternalLinkAlt);

@Component({ components: { Heading } })
export default class RoomCard extends Mixins(HeadingContext) {
  @Prop() readonly room!: Room;

  get empty() {
    return this.users.length === 0;
  }

  get users() {
    return this.$store.getters.roomUsers(this.room.id);
  }

  get isCurrentRoom() {
    return this.$store.getters.currentRoom?.id === this.room.id;
  }

  joinRoom() {
    this.$store.dispatch("joinRoom", this.room.id);
  }

  leaveRoom() {
    this.$store.dispatch("leaveRoom");
  }

  userTip(user: User) {
    const fullName = _.trim(_.join([user.firstName, user.lastName], " "));
    const institution = _.trim(user.institution);
    let html = `<strong>${fullName}</strong>`;
    if (!_.isEmpty(institution)) {
      html += `<br><span class="font-italic">${institution}</span>`;
    }
    return html;
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

.card .user-list {
  margin-bottom: 1rem;
}

.user-list span {
  cursor: default;
  text-decoration: underline dotted;
}
</style>
