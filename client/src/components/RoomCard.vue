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

        <user-list :users="users" />
      </template>

      <icon-button
        v-if="!isCurrentRoom"
        :disabled="isFull"
        icon="video"
        variant="primary"
        @click="joinRoom"
      >
        Join
      </icon-button>
      <div>
        <small v-if="isFull" class="mt-1 text-muted">
          This room is at capacity right now, please join another one.
        </small>
      </div>

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
import UserList from "@/components/UserList.vue";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";
import _ from "lodash";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faComments,
  faVideo,
  faEdit,
  faSave,
  faTimes,
  faDoorOpen
} from "@fortawesome/free-solid-svg-icons";

library.add(faComments, faVideo, faEdit, faSave, faTimes, faDoorOpen);

@Component({ components: { UserList, Heading } })
export default class RoomCard extends Mixins(HeadingContext) {
  readonly topicMaxLength = 50;
  @Prop() readonly room!: Room;

  editingTopic = false;
  saving = false;
  topic = "";

  get isValidTopic() {
    return this.topic.length <= this.topicMaxLength;
  }

  get empty() {
    return this.users.length === 0;
  }

  get users(): User[] {
    return this.$store.getters.roomUsers(this.room.id);
  }

  get isCurrentRoom() {
    return this.$store.getters.currentRoom?.id === this.room.id;
  }

  get currentRoom(): Room | null {
    return this.$store.getters.currentRoom;
  }

  get isFull(): boolean {
    return this.$store.getters.roomIsFull(this.room.id);
  }

  get showLeave(): boolean {
    return this.isCurrentRoom;
  }

  joinRoom() {
    this.$store.dispatch("joinRoom", this.room.id).catch(error => {
      if (error.response?.status == 409) {
        this.showError("Sorry, but this room is already full");
      } else {
        this.showError("An unexpected error happend");
      }
    });
  }

  showError(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Error",
      toaster: "b-toaster-top-center",
      variant: "danger",
      solid: true
    });
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
    this.saving = true;
    this.$store
      .dispatch("updateTopic", { roomId: this.room.id, topic })
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
</style>
