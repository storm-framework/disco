<template>
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

<script lang="ts">
import { Component, Prop, Mixins } from "vue-property-decorator";
import { User } from "@/models";
import UserSummary from "@/components/UserSummary.vue";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";

import { library } from "@fortawesome/fontawesome-svg-core";
import { faDice } from "@fortawesome/free-solid-svg-icons";

library.add(faDice);

@Component({ components: { UserSummary, Heading } })
export default class Lobby extends Mixins(HeadingContext) {
  @Prop() readonly users!: User[];

  selectedUserId: number | null = null;

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
}
</script>

<style lang="scss" scoped>
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
