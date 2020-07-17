<template>
  <article class="call-container" ref="callContainer"></article>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from "vue-property-decorator";
import { Room, User } from "@/models";
import { JitsiRoom } from "./JitsiRoom";

@Component
export default class JitsiCall extends Vue {
  @Prop() room!: Room;
  @Prop() user!: User;

  // There should only ever be multiple rooms for a second when switching
  private jitsiRooms: Map<string, { id: number; room: JitsiRoom }> = new Map();
  private unwatch?: () => void;

  mounted() {
    this.unwatch = this.$watch("room.zoomLink", this.onZoomLinkChanged, {
      immediate: true
    });
  }

  beforeDestroy() {
    this.unwatch?.();
    for (const { room } of this.jitsiRooms.values()) {
      room.disable();
    }
  }

  onZoomLinkChanged(newLink: string, oldLink?: string) {
    const newRoomName = newLink.split("/")[3];

    if (oldLink !== undefined) {
      const oldRoomName = oldLink.split("/")[3];

      if (newRoomName === oldRoomName) {
        return;
      }

      this.jitsiRooms.get(oldRoomName)?.room.disable();
    }

    const newVal = {
      id: this.room.id,
      room: new JitsiRoom(newRoomName, {
        displayName: this.user.displayName,
        avatarUrl: this.user.photoURL ?? ""
      })
    };

    this.jitsiRooms.set(newRoomName, newVal);
    newVal.room.mount(this.$refs.callContainer as Element);

    newVal.room.joined().then(() => {
      this.$emit("joined", newVal.id);
    });

    newVal.room.disabled().then(() => {
      this.$emit("left", newVal.id);
      this.jitsiRooms.delete(newRoomName);
    });
  }

  @Watch("user.displayName", { immediate: true })
  onDisplayNameChanged(displayName: string) {
    for (const { room } of this.jitsiRooms.values()) {
      room.setDisplayName(displayName);
    }
  }

  @Watch("user.photoURL", { immediate: true })
  onAvatarUrlChanged(avatarUrl: string | null) {
    for (const { room } of this.jitsiRooms.values()) {
      room.setAvatarUrl(avatarUrl ?? "");
    }
  }

  @Watch("room.topic", { immediate: true })
  onTopicChanged(topic: string) {
    for (const { room } of this.jitsiRooms.values()) {
      room.setTopic(topic);
    }
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
