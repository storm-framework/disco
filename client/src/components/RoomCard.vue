<template>
  <article :style="{ borderTopColor: room.color }" class="room-card card">
    <b-card-body>
      <heading :level="1" :context="headingContext" class="card-title h4">
        {{ room.name }}
        <span v-if="isCurrentRoom" class="badge badge-secondary">
          Current Room
        </span>
      </heading>

      <div class="mb-3 d-flex flex-row align-items-center">
        <b-badge variant="info" class="mr-2">Topic</b-badge>
        <div v-if="editingTopic" class="form-inline edit-topic">
          <b-form-input
            size="sm"
            class="mr-2"
            v-model="topic"
            :disabled="saving"
          />
          <font-awesome-icon
            icon="save"
            class="text-primary mr-1"
            size="lg"
            @click="saveTopic"
          />
          <font-awesome-icon
            icon="times-circle"
            size="lg"
            @click="editingTopic = false"
            class="text-secondary"
          />
        </div>
        <div v-else>
          <span v-if="hasTopic">{{ room.topic }}</span>
          <span class="font-italic" v-else>No topic</span>
          <a
            href="#"
            class="ml-2"
            v-if="isCurrentRoom"
            @click.prevent="editTopic"
          >
            <font-awesome-icon icon="edit" />
          </a>
        </div>
      </div>

      <b-card-text v-if="isCurrentRoom">
        You are in this room. If you accidentally left the chat,
        <a :href="room.zoomLink" target="_blank">
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

        <b-list-group class="user-list">
          <b-list-group-item
            v-for="user in users"
            :key="user.id"
            @click="toggleExpanded(user)"
          >
            <user-summary
              v-bind="user"
              :long="isExpanded(user)"
              :heading-context="headingContext + 2"
            />
          </b-list-group-item>
        </b-list-group>
      </template>

      <icon-button
        v-if="isCurrentRoom"
        icon="door-open"
        @click="leaveRoom"
        variant="danger"
      >
        Leave
      </icon-button>
      <icon-button
        v-else
        icon="external-link-alt"
        variant="primary"
        @click.passive="joinRoom"
        :href="room.zoomLink"
        target="_blank"
      >
        Join
      </icon-button>
    </b-card-body>
  </article>
</template>

<script lang="ts">
import { Component, Prop, Mixins } from "vue-property-decorator";
import { Room, User } from "@/models";
import UserSummary from "@/components/UserSummary.vue";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";
import _ from "lodash";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faComments,
  faDoorOpen,
  faEdit,
  faExternalLinkAlt,
  faSave,
  faTimesCircle
} from "@fortawesome/free-solid-svg-icons";

library.add(
  faComments,
  faDoorOpen,
  faEdit,
  faExternalLinkAlt,
  faSave,
  faTimesCircle
);

@Component({ components: { UserSummary, Heading } })
export default class RoomCard extends Mixins(HeadingContext) {
  @Prop() readonly room!: Room;
  selectedUserId: string | null = null;

  editingTopic = false;
  saving = false;
  topic = "";

  get empty() {
    return this.users.length === 0;
  }

  get users() {
    return this.$store.getters.roomUsers(this.room.id);
  }

  get isCurrentRoom() {
    return this.$store.getters.currentRoom?.id === this.room.id;
  }

  isExpanded(user: User) {
    return user.id === this.selectedUserId;
  }

  toggleExpanded(user: User) {
    if (this.isExpanded(user)) {
      this.selectedUserId = null;
    } else {
      this.selectedUserId = user.id;
    }
  }

  joinRoom() {
    this.$store.dispatch("joinRoom", this.room.id);
  }

  leaveRoom() {
    this.$store.dispatch("leaveRoom");
  }

  editTopic() {
    this.topic = this.room.topic;
    this.editingTopic = true;
  }

  saveTopic() {
    if (this.saving) {
      return;
    }
    const topic = _.trim(this.topic);
    const data = { ...this.room, topic };
    this.saving = true;
    this.$store
      .dispatch("updateRoom", { roomId: this.room.id, data })
      .finally(() => {
        this.saving = false;
        this.editingTopic = false;
      });
  }

  get hasTopic() {
    return !_.isEmpty(_.trim(this.room.topic));
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
.room-card {
  overflow: hidden;
  border-top-width: 1rem;
}

.badge {
  overflow: hidden;
  vertical-align: -0.25em;
}

.user-list {
  margin-bottom: 1rem;
}

.user-list span {
  cursor: default;
  text-decoration: underline dotted;
}

.edit-topic {
  display: flex;
  flex: auto;
}

.edit-topic input {
  flex: auto;
  height: 24px;
}
</style>
