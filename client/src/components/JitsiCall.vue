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
    this.call.on("joined", () => {
      this.$emit("joined", this.room.id);
    });

    this.call.on("left", () => {
      this.$emit("left", this.room.id);
    });
  }

  mounted() {
    this.call.setParent(this.$refs.callContainer as Element);
    this.call.enable();
  }

  beforeDestroy() {
    this.call.disable();
  }

  @Watch("room.zoomLink", { immediate: true })
  onLinkChanged(zoomLink: string) {
    const roomName = zoomLink.split("/")[3];
    this.call.setRoom(roomName);
  }

  @Watch("user.displayName", { immediate: true })
  onDisplayNameChanged(displayName: string) {
    this.call.setDisplayName(displayName);
  }

  @Watch("user.photoURL", { immediate: true })
  onAvatarUrlChanged(avatarUrl: string | null) {
    this.call.setAvatarUrl(avatarUrl);
  }

  @Watch("room.topic", { immediate: true })
  onTopicChanged(topic: string) {
    this.call.setTopic(topic);
  }
}
</script>

<style lang="scss" scoped>
.call-container {
  background: var(--dark);
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
