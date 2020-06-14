<template>
  <article class="callContainer" ref="callContainer"></article>
</template>

<script lang="ts">
import { Vue, Component, Prop } from "vue-property-decorator";

const JITSI_CONFIG = {
  disableInviteFunctions: true,
  doNotStoreRoom: true
};

const JITSI_INTERFACE_CONFIG = {
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
    "filmstrip",
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

interface JitsiMeetConfig {
  disableInviteFunctions: boolean;
  doNotStoreRoom: boolean;
  // ...
}

interface JitsiMeetInterfaceConfig {
  TOOLBAR_BUTTONS: string[];
  SETTINGS_SECTIONS: string[];
  RECENT_LIST_ENABLED: boolean;
  // ...
}

interface JitsiMeetUserInfo {
  displayName: string;
  email: string;
  avatarURL: string;
}

interface JitsiMeetApiOptions {
  roomName?: string;
  width?: number | string;
  height?: number | string;
  parentNode?: Element;
  configOverwrite?: Partial<Config>;
  interfaceConfigOverwrite?: Partial<InterfaceConfig>;
  noSSL?: boolean;
  jwt?: string;
  onload?: EventHandlerNonNull;
  invitees?: object[];
  devices?: object;
  userInfo?: Partial<UserInfo>;
}

declare global {
  class JitsiMeetExternalAPI {
    constructor(domain: string, options: JitsiMeetApiOptions);
  }
}


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
  height: 100vh;
}
</style>
