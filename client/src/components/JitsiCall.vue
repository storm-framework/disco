<template>
  <article class="call-container" ref="callContainer"></article>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from "vue-property-decorator";
import { Room, User } from "@/models";
import Call from "./Call";

@Component
export default class JitsiCall extends Vue {
  @Prop() room!: Room;
  @Prop() user!: User;

  private call: Call = new Call();

  created() {
    this.call.on("roomJoined", () => {
      console.log(`emitting room-joined ${this.room.id}`);
      this.$emit("room-joined", this.room.id);
    });

    this.call.on("roomLeft", () => {
      console.log(`emitting room-left ${this.room.id}`);
      this.$emit("room-left", this.room.id);
    });
  }

  mounted() {
    this.call.parent = this.$refs.callContainer as Element;
    this.call.open();
  }

  beforeDestroy() {
    this.call.close();
  }

  @Watch("room.zoomLink", { immediate: true })
  onLinkChanged(zoomLink: string) {
    this.call.roomName = zoomLink.split("/")[3];
  }

  @Watch("user.displayName", { immediate: true })
  onDisplayNameChanged(displayName: string) {
    this.call.displayName = displayName;
  }

  @Watch("user.photoURL", { immediate: true })
  onAvatarUrlChanged(avatarUrl: string | null) {
    this.call.avatarUrl = avatarUrl || "";
  }

  @Watch("room.topic", { immediate: true })
  onTopicChanged(topic: string) {
    this.call.subject = topic;
  }

}
</script>

<style lang="scss" scoped>
.call-container {
  position: relative;
  overflow: hidden;

  iframe {
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
  }
}
</style>
