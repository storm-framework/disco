<template>
  <article class="lobby card">
    <b-card-body>
      <heading :level="1" :context="headingContext" class="card-title h4">
        Lobby
      </heading>

      <heading :level="2" :context="headingContext" class="sr-only">
        In the Lobby
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
      <icon-button icon="dice" variant="primary" @click="joinRandomRoom">
        Random Room
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
import { faDice } from "@fortawesome/free-solid-svg-icons";

library.add(faDice);

@Component({ components: { UserSummary, Heading } })
export default class Lobby extends Mixins(HeadingContext) {
  readonly topicMaxLength = 50;
  @Prop() readonly room!: Room;
  selectedUserId: number | null = null;

  get users() {
    return this.$store.getters.lobbyUsers;
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

  showError(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Error",
      toaster: "b-toaster-top-center",
      variant: "danger",
      solid: true
    });
  }

  joinRandomRoom() {
    const random: Room | null = _.sample(this.$store.getters.availableRooms);
    if (random) {
      this.$store.dispatch("joinRoom", random.id).catch(error => {
        if (error.response?.status == 409) {
          this.showError("Sorry, but the room is already full");
        } else {
          this.showError("An unexpected error happend");
        }
      });
    }
  }
}
</script>

<style lang="scss" scoped>
.lobby {
  overflow: hidden;
  border-top-width: 1rem;
  min-width: 300px;
}

.user-list {
  margin-bottom: 1rem;
}

.user-list span {
  cursor: default;
  text-decoration: underline dotted;
}

.list-group-item.compact {
  padding: 0.5rem 1rem;
}
</style>
