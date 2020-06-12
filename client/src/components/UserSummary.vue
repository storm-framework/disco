<template>
  <div>
    <div class="user-brief" v-if="!long">
      <b-avatar class="photo" :src="photoURL" :text="avatarText" size="2em" />
      <p class="name">{{ displayName }}</p>
    </div>
    <b-media v-else>
      <template v-slot:aside>
        <b-avatar class="photo" :src="photoURL" :text="avatarText" size="6em" />
      </template>
      <heading :level="1" :context="headingContext">
        {{ displayName }}
        <router-link
          class="edit-profile"
          :to="{ name: 'Profile' }"
          v-if="editable"
        >
          <font-awesome-icon icon="edit" />
        </router-link>
      </heading>
      <dl>
        <div v-if="pronouns" class="info-item">
          <dt>Pronouns</dt>
          <dd>{{ pronouns }}</dd>
        </div>
        <div v-if="affiliation" class="info-item">
          <dt>Affiliation</dt>
          <dd>{{ institution }}</dd>
        </div>
        <div v-if="website" class="info-item">
          <dt>Website</dt>
          <dd>
            <a :href="website">{{ website }}</a>
          </dd>
        </div>
      </dl>
      <p v-if="bio" class="bio">{{ bio }}</p>
    </b-media>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Mixins } from "vue-property-decorator";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";
import _ from "lodash";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faUser,
  faUniversity,
  faLink,
  faEdit
} from "@fortawesome/free-solid-svg-icons";

library.add(faUser, faUniversity, faLink, faEdit);

@Component({ components: { Heading } })
export default class UserSummary extends Mixins(HeadingContext) {
  @Prop({ type: Boolean, default: false }) editable!: boolean;

  @Prop({ required: true }) displayName!: string;

  @Prop({ type: Boolean, default: false }) long!: boolean;

  @Prop({ default: "" })
  readonly photoURL!: string;

  @Prop({ default: "" })
  readonly pronouns!: string;

  @Prop({ default: "" })
  readonly institution!: string;

  @Prop({ default: "" })
  readonly website!: string;

  @Prop({ default: "" })
  readonly bio!: string;

  get avatarText(): string {
    return this.displayName.slice(0, 2);
  }
}
</script>

<style lang="scss" scoped>
.info-item {
  dt,
  dd {
    display: inline;
  }

  dt {
    margin-right: 0.5em;
  }
}

.bio {
  max-width: 20em;
}

.user-brief {
  display: flex;
  align-items: center;

  .name {
    margin-bottom: 0;
    margin-left: 1em;
  }
}

.edit-profile {
  vertical-align: super;
  font-size: 16px;
}
</style>
