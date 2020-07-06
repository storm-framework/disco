<template>
  <article class="callContainer" ref="callContainer"></article>
</template>

<script lang="ts">
import { Vue, Component, Prop } from "vue-property-decorator";
import JitsiMeetExternalAPI, {
  JitsiConfig,
  JitsiInterfaceConfig
} from "jitsi-meet-external-api";

const JITSI_CONFIG: JitsiConfig = {
  disableInviteFunctions: true,
  doNotStoreRoom: true
};

const JITSI_INTERFACE_CONFIG: JitsiInterfaceConfig = {
  TOOLBAR_BUTTONS: [
    "microphone",
    "camera",
    "closedcaptions",
    "desktop",
    "fullscreen",
    "fodeviceselection",
    "hangup",
    "chat",
    "settings",
    "raisehand",
    "videoquality",
    "feedback",
    "stats",
    "shortcuts",
    "tileview",
    "videobackgroundblur",
    "help"
  ],
  SETTINGS_SECTIONS: ["devices", "language"],
  RECENT_LIST_ENABLED: false
};

@Component
export default class JitsiCall extends Vue {
  @Prop() roomName?: string;
  @Prop() displayName?: string;
  @Prop() avatarURL?: string;

  private api?: JitsiMeetExternalAPI;

  mounted() {
    this.createCall();
  }

  private createCall() {
    this.api = new JitsiMeetExternalAPI("meet.jit.si", {
      roomName: this.roomName,
      parentNode: this.$refs.callContainer as Element,
      configOverwrite: JITSI_CONFIG,
      interfaceConfigOverwrite: JITSI_INTERFACE_CONFIG,
      userInfo: {
        displayName: this.displayName,
        avatarURL: this.avatarURL
      }
    });
  }
}
</script>

<style lang="scss" scoped>
.callContainer {
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
