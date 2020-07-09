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
      this.$emit("room-joined", this.room.id);
    });

    this.call.on("roomLeft", () => {
      this.$emit("room-left", this.room.id);
    });
  }

  mounted() {
    this.call.on("iframeChanged", this.injectIframe);
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

  private injectIframe(iframe: HTMLIFrameElement) {
    const callContainer: Element = this.$refs.callContainer as Element;
    while (callContainer.firstChild) {
      callContainer.removeChild(callContainer.firstChild);
    }
    callContainer.appendChild(iframe);
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
