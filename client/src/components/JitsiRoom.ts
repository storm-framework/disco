import JitsiMeetExternalAPI, {
  AppConfig,
  Events,
  Commands,
  InterfaceConfig,
  ParticipantId
} from "jitsi-meet-external-api";

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

interface Disabled {
  label: "disabled";
}

interface Mounted {
  label: "mounted";
  api: JitsiMeetExternalAPI;
}

interface Joined {
  label: "joined";
  api: JitsiMeetExternalAPI;
  id: ParticipantId;
}

interface Left {
  label: "left";
  api: JitsiMeetExternalAPI;
}

export class JitsiRoom {
  readonly roomName: string;
  readonly domain: string;

  private displayName: string;
  private avatarUrl: string;
  private topic: string;

  private state: Disabled | Mounted | Joined | Left = { label: "disabled" };
  private readonly listeners: {
    joined: (() => void)[];
    disabled: (() => void)[];
    left: (() => void)[];
  } = { joined: [], disabled: [], left: [] };

  constructor(
    roomName: string,
    {
      domain,
      displayName,
      avatarUrl,
      topic
    }: Partial<{
      domain: string;
      displayName: string;
      avatarUrl: string;
      topic: string;
    }>
  ) {
    this.roomName = roomName;
    this.domain = domain ?? "meet.jit.si";
    this.displayName = displayName ?? "";
    this.avatarUrl = avatarUrl ?? "";
    this.topic = topic ?? "";
  }

  private setDisabled() {
    switch (this.state.label) {
      case "mounted":
      case "left":
        this.state = { label: "disabled" };
        for (const callback of this.listeners.disabled) {
          callback();
        }
        this.listeners.disabled = [];
        break;

      default:
        throw new Error(
          `Invalid state transition from ${this.state.label} to disabled`
        );
    }
  }

  private setMounted(api: JitsiMeetExternalAPI) {
    switch (this.state.label) {
      case "disabled":
        this.state = { label: "mounted", api };
        break;

      default:
        throw new Error(
          `Invalid state transition from ${this.state.label} to mounted`
        );
    }
  }

  private setJoined(id: ParticipantId) {
    switch (this.state.label) {
      case "mounted":
        this.state = { label: "joined", api: this.state.api, id };
        for (const callback of this.listeners.joined) {
          callback();
        }
        this.listeners.joined = [];
        break;

      default:
        throw new Error(
          `Invalid state transition from ${this.state.label} to joined`
        );
    }
  }

  private setLeft() {
    switch (this.state.label) {
      case "joined":
        this.state = { label: "left", api: this.state.api };
        for (const callback of this.listeners.left) {
          callback();
        }
        this.listeners.left = [];
        break;

      default:
        throw new Error(
          `Invalid state transition from ${this.state.label} to left`
        );
    }
  }

  async setDisplayName(displayName: string) {
    if (displayName !== this.displayName) {
      this.displayName = displayName;
      await this.refreshDisplayName();
    }
  }

  async setAvatarUrl(avatarUrl: string) {
    if (avatarUrl !== this.avatarUrl) {
      this.avatarUrl = avatarUrl;
      await this.refreshAvatarUrl();
    }
  }

  async setTopic(topic: string) {
    if (topic !== this.topic) {
      this.topic = topic;
      await this.refreshTopic();
    }
  }

  mount(parent: Element) {
    if (this.state.label !== "disabled") {
      throw new Error("Already mounted");
    }

    const api = new JitsiMeetExternalAPI(this.domain, {
      parentNode: parent,
      configOverwrite: APP_CONFIG,
      interfaceConfigOverwrite: INTERFACE_CONFIG,
      roomName: this.roomName,
      userInfo: {
        displayName: this.displayName,
        avatarURL: this.avatarUrl
      }
    });

    api.on("videoConferenceJoined", ({ id, displayName, avatarURL }) => {
      this.setJoined(id);

      if (displayName !== this.displayName) {
        this.refreshDisplayName();
      }
      if (avatarURL !== this.avatarUrl) {
        this.refreshAvatarUrl();
      }
    });

    api.on("videoConferenceLeft", () => {
      if (this.state.label === "joined") {
        this.setLeft();
      }
    });

    this.left().then(() => {
      this.disable();
    });

    this.setMounted(api);
  }

  async leave() {
    if (this.state.label === "joined") {
      this.state.api.executeCommand("hangup");
      await this.left();
    }
  }

  async disable() {
    await this.leave();

    if (this.state.label !== "disabled") {
      this.state.api.dispose();
      this.setDisabled();
    }
  }

  joined(): Promise<void> {
    if (this.state.label === "joined") {
      return Promise.resolve();
    }
    return new Promise(resolve => {
      this.listeners.joined.push(resolve);
    });
  }

  private left(): Promise<void> {
    if (this.state.label === "left") {
      return Promise.resolve();
    }
    return new Promise(resolve => {
      this.listeners.left.push(resolve);
    });
  }

  disabled(): Promise<void> {
    if (this.state.label === "disabled") {
      return Promise.resolve();
    }
    return new Promise(resolve => {
      this.listeners.disabled.push(resolve);
    });
  }

  private command<Command extends keyof Commands, Event extends keyof Events>(
    command: Command,
    {
      event,
      filter
    }: {
      event: Event;
      filter?: (...args: Parameters<Events[Event]>) => boolean;
    },
    ...commandArgs: Parameters<Commands[Command]>
  ): Promise<void> {
    if (this.state.label === "disabled") {
      return Promise.reject(new Error("Call is disabled"));
    }

    const api = this.state.api;

    return new Promise(resolve => {
      // I don't know why I have to cast this.
      const listener = ((...handlerArgs: Parameters<Events[Event]>) => {
        if (filter?.(...handlerArgs) ?? true) {
          api.removeListener(event, listener);
          resolve();
        }
      }) as Events[Event];

      api.addListener(event, listener);
      api.executeCommand(command, ...commandArgs);
    });
  }

  private async refreshDisplayName() {
    if (this.state.label === "joined") {
      const myId = this.state.id;
      const myDisplayName = this.displayName;

      await this.command(
        "displayName",
        {
          event: "displayNameChange",
          filter: ({ id, displayname }) =>
            id === myId && displayname == myDisplayName
        },
        myDisplayName
      );
    }
  }

  private async refreshAvatarUrl() {
    if (this.state.label === "joined") {
      const myId = this.state.id;
      const myAvatarUrl = this.avatarUrl;

      await this.command(
        "avatarUrl",
        {
          event: "avatarChanged",
          filter: ({ id, avatarURL }) => id === myId && avatarURL == myAvatarUrl
        },
        myAvatarUrl
      );
    }
  }

  private async refreshTopic() {
    if (this.state.label === "joined") {
      const myTopic = this.topic;

      await this.command(
        "subject",
        {
          event: "subjectChange",
          filter: ({ subject }) => subject === myTopic
        },
        myTopic
      );
    }
  }
}
