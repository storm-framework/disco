import JitsiMeetExternalAPI, {
  AppConfig,
  InterfaceConfig
} from "jitsi-meet-external-api";
import { TypedEmitter } from "tiny-typed-emitter";

const APP_CONFIG: AppConfig = {
  disableInviteFunctions: true,
  doNotStoreRoom: true,
  enableClosePage: true,
  transcribingEnabled: true
};

const INTERFACE_CONFIG: InterfaceConfig = {
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
    "stats",
    "shortcuts",
    "tileview",
    "videobackgroundblur",
    "help"
  ],
  SETTINGS_SECTIONS: ["devices", "language"],
  RECENT_LIST_ENABLED: false
};

export interface CallOptions {
  domain?: string;
  roomName?: string;
  displayName?: string;
  avatarUrl?: string;
  subject?: string;
}

interface Events {
  roomJoined(roomName: string): void;
  roomLeft(roomName: string): void;
}

export default class Call extends TypedEmitter<Events> {
  private readonly domain: string;
  parent: Element | null = null;
  private api: JitsiMeetExternalAPI | null = null;
  private isOpen = false;
  private joined = false;
  private currentDisplayName: string;
  private currentAvatarUrl: string;
  private currentRoomName: string;
  private currentSubject: string;

  constructor(options?: CallOptions) {
    super();
    this.domain = options?.domain || "meet.jit.si";
    this.currentDisplayName = options?.displayName || "";
    this.currentAvatarUrl = options?.avatarUrl || "";
    this.currentRoomName = options?.roomName || "";
    this.currentSubject = options?.subject || "";
  }

  get displayName(): string {
    return this.currentDisplayName;
  }

  set displayName(displayName: string) {
    if (displayName !== this.currentDisplayName) {
      this.currentDisplayName = displayName;
      this.api?.executeCommand("displayName", displayName);
    }
  }

  get avatarUrl(): string {
    return this.currentAvatarUrl;
  }

  set avatarUrl(avatarUrl: string) {
    if (avatarUrl !== this.currentAvatarUrl) {
      this.currentAvatarUrl = avatarUrl;
      this.api?.executeCommand("avatarUrl", avatarUrl);
    }
  }

  get roomName(): string {
    return this.currentRoomName;
  }

  set roomName(roomName: string) {
    if (roomName !== this.currentRoomName) {
      this.currentRoomName = roomName;
      if (this.isOpen) {
        this.open();
      }
    }
  }

  get subject(): string {
    return this.currentSubject;
  }

  set subject(subject: string) {
    if (subject !== this.currentSubject) {
      this.currentSubject = subject;
      this.api?.executeCommand("subject", subject);
    }
  }

  open() {
    if (this.roomName && this.parent) {
      const api = new JitsiMeetExternalAPI(this.domain, {
        parentNode: this.parent,
        configOverwrite: APP_CONFIG,
        interfaceConfigOverwrite: INTERFACE_CONFIG,
        roomName: this.roomName,
        userInfo: {
          displayName: this.displayName,
          avatarURL: this.avatarUrl
        }
      });

      api.on("videoConferenceJoined", ({ roomName }) => {
        this.emit("roomJoined", roomName);
        api.executeCommand("subject", this.subject);
        this.joined = true;
      });
      api.on("videoConferenceLeft", ({ roomName }) => {
        this.emit("roomLeft", roomName);
        this.joined = false;
      });

      api.on("readyToClose", () => { api.dispose(); });

      this.api = api;
      this.isOpen = true;
    } else {
      this.close();
    }
  }

  close() {
    if (this.joined) {
      this.api?.executeCommand("hangup");
    }
    this.api = null;
    this.isOpen = false;
  }
}
