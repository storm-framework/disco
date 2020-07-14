import JitsiMeetExternalAPI, {
  AppConfig,
  Events as ApiEvents,
  InterfaceConfig,
  ParticipantId,
  UserInfo
} from "jitsi-meet-external-api";
import { TypedEmitter } from "tiny-typed-emitter";

const APP_CONFIG: AppConfig = {
  disableInviteFunctions: true,
  doNotStoreRoom: true,
  enableClosePage: true,
  transcribingEnabled: true,
  prejoinPageEnabled: false
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

interface Events {
  joined(): void;
  left(): void;
}

interface StateDisabled {
  state: "disabled";
}

interface StateSetup {
  state: "setup";
  api: JitsiMeetExternalAPI;
}

interface StateActive {
  state: "active";
  participantId: ParticipantId;
  api: JitsiMeetExternalAPI;
}

interface StateTeardown {
  state: "teardown";
  api: JitsiMeetExternalAPI;
}

type State = StateDisabled | StateSetup | StateActive | StateTeardown;

function expectEvent<ApiEvent extends keyof ApiEvents>(
  api: JitsiMeetExternalAPI,
  event: ApiEvent,
  predicate?: (...args: Parameters<ApiEvents[ApiEvent]>) => boolean
): Promise<void> {
  return new Promise(resolve => {
    const test = predicate || (() => true);

    const listener = ((...args: Parameters<ApiEvents[ApiEvent]>) => {
      if (test(...args)) {
        api.removeListener(event, listener);
        resolve();
      }
    }) as ApiEvents[ApiEvent];

    api.addListener(event, listener);
  });
}

export default class Call extends TypedEmitter<Events> {
  private readonly domain: string;

  private state: State = { state: "disabled" };

  private parent: Element | null = null;
  private displayName: string | null = null;
  private avatarUrl: string | null = null;
  private room: string | null = null;
  private topic: string | null = null;

  constructor(domain?: string) {
    super();
    this.domain = domain || "meet.jit.si";
  }

  setParent(parent: Element): void {
    if (parent !== this.parent) {
      this.parent = parent;
      this.updateParent();
    }
  }

  updateParent(): void {
    if (this.state.state !== "disabled") {
      if (!this.parent) {
        this.disable();
      } else {
        const iframe = this.state.api.getIFrame();
        this.parent.appendChild(iframe);
      }
    }
  }

  async setDisplayName(displayName: string) {
    if (displayName !== this.displayName) {
      this.displayName = displayName;
      await this.updateDisplayName();
    }
  }

  async updateDisplayName() {
    if (this.state.state === "active") {
      const { api, participantId } = this.state;
      const updated = expectEvent(
        api,
        "displayNameChange",
        ({ id, displayname }) =>
          id === participantId && displayname === this.displayName
      );
      api.executeCommand("displayName", this.displayName || "");
      await updated;
    }
  }

  async setAvatarUrl(avatarUrl: string | null) {
    if (avatarUrl !== this.avatarUrl) {
      this.avatarUrl = avatarUrl;
      await this.updateAvatarUrl();
    }
  }

  async updateAvatarUrl() {
    if (this.state.state === "active") {
      const { api, participantId } = this.state;
      const updated = expectEvent(
        api,
        "avatarChanged",
        ({ id, avatarURL }) =>
          id === participantId && avatarURL === this.avatarUrl
      );
      api.executeCommand("avatarUrl", this.avatarUrl || "");
      await updated;
    }
  }

  async setTopic(topic: string) {
    if (topic !== this.topic) {
      this.topic = topic;
      await this.updateTopic();
    }
  }

  async updateTopic() {
    if (this.state.state === "active") {
      const { api } = this.state;
      const updated = expectEvent(
        api,
        "subjectChange",
        ({ subject }) => subject === this.topic
      );
      api.executeCommand("subject", this.topic || "");
      await updated;
    }
  }

  async setRoom(room: string) {
    if (room !== this.room) {
      this.room = room;
      this.updateRoom();
    }
  }

  async updateRoom() {
    if (this.state.state !== "disabled") {
      await this.disable();
      await this.enable();
    }
  }

  async enable() {
    if (this.state.state !== "disabled") {
      return;
    }

    if (!this.room || !this.parent) {
      throw new Error(
        "You need to set a room and a parent before you can enable the call"
      );
    }

    const userInfo: Partial<UserInfo> = {};

    if (this.displayName) {
      userInfo.displayName = this.displayName;
    }

    if (this.avatarUrl) {
      userInfo.avatarURL = this.avatarUrl;
    }

    const api = new JitsiMeetExternalAPI(this.domain, {
      parentNode: this.parent,
      configOverwrite: APP_CONFIG,
      interfaceConfigOverwrite: INTERFACE_CONFIG,
      roomName: this.room,
      userInfo
    });

    api.once(
      "videoConferenceJoined",
      ({ roomName, id, displayName, avatarURL }) => {
        this.emit("joined");

        this.state = {
          state: "active",
          participantId: id,
          api
        };

        if (roomName !== this.room) {
          this.updateRoom();
        } else {
          if (displayName !== this.displayName) {
            this.updateDisplayName();
          }

          if (avatarURL !== this.avatarUrl) {
            this.updateAvatarUrl();
          }
        }
      }
    );

    api.once("videoConferenceLeft", () => {
      this.emit("left");

      this.state = {
        state: "teardown",
        api
      };
    });

    api.once("readyToClose", () => {
      this.disable();
    });
  }

  async disable() {
    if (this.state.state === "disabled") {
      return;
    }

    if (this.state.state === "active") {
      const left = expectEvent(
        this.state.api,
        "videoConferenceLeft",
        () => true
      );
      this.state.api.executeCommand("hangup");
      await left;
    }

    this.state.api.dispose();
    this.state = { state: "disabled" };
  }
}
