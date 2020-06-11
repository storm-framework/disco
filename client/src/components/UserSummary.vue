<template>
  <div>
    <div class="user-brief" v-if="!long">
      <b-avatar class="photo" :src="photoURL" :text="avatarText" size="2em" />
      <p class="name">{{ name }}</p>
    </div>
    <b-media v-else>
      <template v-slot:aside>
        <b-avatar class="photo" :src="photoURL" :text="avatarText" size="6em" />
      </template>
      <heading :level="1" :context="headingContext">
        {{ name }}
      </heading>
      <dl>
        <div class="info-item">
          <dt>Pronouns</dt>
          <dd>{{ pronouns }}</dd>
        </div>
        <div class="info-item">
          <dt>Affiliation</dt>
          <dd>{{ institution }}</dd>
        </div>
        <div class="info-item">
          <dt>Website</dt>
          <dd>
            <a :href="website">{{ website }}</a>
          </dd>
        </div>
      </dl>
      <p>{{ bio }}</p>
    </b-media>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Mixins } from "vue-property-decorator";
import HeadingContext from "@/mixins/HeadingContext";
import Heading from "@/components/Heading";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faUser,
  faUniversity,
  faLink
} from "@fortawesome/free-solid-svg-icons";

library.add(faUser, faUniversity, faLink);

@Component({ components: { Heading } })
export default class UserSummary extends Mixins(HeadingContext) {
  // TODO: Remove firstName and lastName
  @Prop({ default: "Foobar" })
  readonly firstName!: string;

  @Prop({ default: "Quuxlington" })
  readonly lastName!: string;

  get name(): string {
    return `${this.firstName} ${this.lastName}`;
  }

  @Prop({ type: Boolean, default: false })
  long: boolean;

  @Prop({ default: null })
  readonly photoURL!: string | null;

  @Prop({
    default: "they/them" // TODO: default null
  })
  readonly pronouns!: string | null;

  @Prop({
    default: "PLMW" // TODO: default null
  })
  readonly institution!: string | null;

  @Prop({
    default: "example.com" // TODO: default null
  })
  readonly website!: string | null;

  @Prop({
    // TODO: default null
    default:
      "Doloribus rem voluptas omnis occaecati et ea. Rerum aspernatur ex fugit quam. Est culpa et quia et beatae et. Quibusdam ut illum illo modi magni maxime. Rerum modi error deleniti provident."
  })
  readonly bio!: string | null;

  get avatarText(): string {
    return this.name.slice(0, 2);
  }

  get avatarSize(): string {
    if (this.extended) {
      return "6em";
    } else {
      return "2.5em";
    }
  }
}
</script>

<style lang="scss" scoped>
.info-item {
  display: inline-block;
  margin-right: 1em;

  dt,
  dd {
    display: inline;
  }

  dt {
    margin-right: 0.5em;
  }
}

.user-brief {
  display: flex;
  align-items: center;

  .name {
    margin-bottom: 0;
    margin-left: 1em;
  }
}
</style>
