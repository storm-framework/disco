declare module "jitsi-meet-external-api" {
  export interface JitsiConfig {
    disableInviteFunctions?: boolean;
    doNotStoreRoom?: boolean;
    // ...
  }

  export interface JitsiInterfaceConfig {
    TOOLBAR_BUTTONS?: string[];
    SETTINGS_SECTIONS?: string[];
    RECENT_LIST_ENABLED?: boolean;
    // ...
  }

  export interface JitsiUserInfo {
    displayName: string;
    email: string;
    avatarURL: string;
  }

  export interface JitsiOptions {
    roomName?: string;
    width?: number | string;
    height?: number | string;
    parentNode?: Element;
    configOverwrite?: JitsiConfig;
    interfaceConfigOverwrite?: JitsiInterfaceConfig;
    noSSL?: boolean;
    jwt?: string;
    onload?: EventHandlerNonNull;
    invitees?: object[];
    devices?: object;
    userInfo?: Partial<JitsiUserInfo>;
  }

  export default class JitsiMeetExternalAPI {
    constructor(domain: string, options: JitsiOptions);
  }
}
