<template>
  <article :style="{ borderTopColor: room.color }" class="room-card card">
    <b-card-body>
      <heading :level="1" :context="headingContext" class="card-title h4">
        {{ room.name }}
        <span v-if="isCurrentRoom" class="badge badge-secondary">
          Current Room
        </span>
      </heading>

      <b-row class="mb-3 align-items-center" no-gutters>
        <b-col cols="auto" class="mr-2">
          <b-badge variant="info">Topic</b-badge>
        </b-col>
        <div v-if="editingTopic" class="form-inline edit-topic">
          <b-form-input
            size="sm"
            class="mr-2"
            v-model="topic"
            autofocus
            :disabled="saving"
            :state="isValidTopic && null"
          />
          <font-awesome-icon
            icon="save"
            class="text-primary mr-2 action"
            size="lg"
            @click="saveTopic"
          />
          <font-awesome-icon
            icon="times"
            size="lg"
            @click="editingTopic = false"
            class="text-secondary action"
          />
        </div>
        <div v-else class="col">
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
      </b-row>

      <b-card-text v-if="empty">
        The room is empty.
      </b-card-text>
      <template v-else>
        <heading :level="2" :context="headingContext" class="sr-only">
          In this room
        </heading>

        <b-list-group class="user-list">
          <b-list-group-item
            button
            v-for="user in users"
            :key="user.id"
            @click="toggleExpanded(user)"
            class="compact"
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
        v-if="showJoin"
        icon="phone"
        variant="primary"
        @click="joinRoom"
      >
        Join
      </icon-button>

      <icon-button
        v-if="showLeave"
        icon="door-open"
        variant="danger"
        @click="leaveRoom"
      >
        Leave
      </icon-button>

      <p v-if="!showLeave && isCurrentRoom">
        <i>Leave</i> in the video pane to exit
      </p>
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
  faPhone,
  faEdit,
  faExternalLinkAlt,
  faSave,
  faTimes,
  faDoorOpen
} from "@fortawesome/free-solid-svg-icons";

library.add(
  faComments,
  faPhone,
  faEdit,
  faExternalLinkAlt,
  faSave,
  faTimes,
  faDoorOpen
);

@Component({ components: { UserSummary, Heading } })
export default class RoomCard extends Mixins(HeadingContext) {
  readonly topicMaxLength = 50;
  @Prop() readonly room!: Room;
  selectedUserId: number | null = null;

  editingTopic = false;
  saving = false;
  topic = "";

  get isValidTopic() {
    return this.topic.length <= this.topicMaxLength;
  }

  get empty() {
    return this.users.length === 0;
  }

  get users() {
    return this.$store.getters.roomUsers(this.room.id);
  }

  get isCurrentRoom() {
    return this.$store.getters.currentRoom?.id === this.room.id;
  }

  get currentRoom() {
    return this.$store.getters.currentRoom;
  }

  get showJoin(): boolean {
    const current = this.$store.getters.currentRoom;
    const video = this.$store.getters.isVideoRoom(current);
    return !current || (!video && !this.isCurrentRoom);
  }

  get showLeave(): boolean {
    const current = this.$store.getters.currentRoom;
    const video = this.$store.getters.isVideoRoom(current);
    return !this.showJoin && this.isCurrentRoom; // && !video;
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
    this.$store.dispatch("leaveRoom", this.room.id);
  }

  editTopic() {
    this.topic = this.room.topic;
    this.editingTopic = true;
  }

  saveTopic() {
    if (this.saving) {
      return;
    }
    const topic = _.trim(this.topic).substring(0, this.topicMaxLength);
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
}
</script>

<style lang="scss" scoped>
.room-card {
  overflow: hidden;
  border-top-width: 1rem;
  min-width: 300px;
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

.edit-topic .action {
  cursor: pointer;
}

.list-group-item.compact {
  padding: 0.5rem 1rem;
}
</style>
