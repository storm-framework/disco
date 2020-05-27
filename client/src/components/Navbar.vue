<template>
  <div>
    <b-navbar toggleable="lg" type="dark" variant="info">
      <b-container>
        <b-navbar-brand to="/">Community Video</b-navbar-brand>

        <b-navbar-nav class="ml-auto">
          <b-nav-item v-on:click="signOut">Sign out</b-nav-item>
        </b-navbar-nav>
      </b-container>
    </b-navbar>
    <loading-progress
      :indeterminate="activity"
      shape="line"
      height="5"
      fillDuration="3"
      :size="windowWidth"
      hideBackground
      class="navbar-activity-indicator"
    />
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";
import { vueWindowSizeMixin } from "vue-window-size";

@Component({
  mixins: [vueWindowSizeMixin]
})
export default class Navbar extends Vue {
  @Prop({ default: false }) activity!: boolean;

  signOut() {
    this.$store
      .dispatch("signOut")
      .then(() => this.$router.push({ name: "SignIn" }));
  }
}
</script>

<style lang="scss">
.navbar-activity-indicator {
  position: absolute;
  top: -11px;
  left: -25px;
}

.navbar-activity-indicator .progress {
  stroke: gray;
}
</style>
