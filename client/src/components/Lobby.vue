<template>
  <article class="lobby card">
    <b-card-body>
      <heading :level="1" :context="headingContext" class="card-title h4">
        Lobby
        <span v-if="inLobby" class="badge badge-secondary">
          Current Room
        </span>
      </heading>

      <heading :level="2" :context="headingContext" class="sr-only">
        In the Lobby
      </heading>

      <b-card-text v-if="empty">
        The lobby is empty.
      </b-card-text>
      <template v-else>
        <heading :level="2" :context="headingContext" class="sr-only">
          In the lobby
        </heading>

        <user-list :users="users" />
      </template>

      <icon-button icon="dice" variant="primary" @click="joinRandomRoom">
        Random Room
      </icon-button>
    </b-card-body>
  </article>
</template>

<script lang="ts">
import { Component, Mixins } from "vue-property-decorator";
import { User } from "../models";
import UserList from "@/components/UserList.vue";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";

import { library } from "@fortawesome/fontawesome-svg-core";
import { faDice } from "@fortawesome/free-solid-svg-icons";

library.add(faDice);

@Component({ components: { UserList, Heading } })
export default class Lobby extends Mixins(HeadingContext) {
  readonly topicMaxLength = 50;

  get inLobby() {
    return this.$store.getters.currentRoom === null;
  }

  get users(): User[] {
    return this.$store.getters.lobbyUsers;
  }

  get empty() {
    return this.users.length === 0;
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
    this.$store.dispatch("joinRandom").catch(error => {
      if (error.response?.status == 409) {
        this.showError("Sorry, but all rooms are currently full");
      } else {
        this.showError("An unexpected error happend");
      }
    });
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
