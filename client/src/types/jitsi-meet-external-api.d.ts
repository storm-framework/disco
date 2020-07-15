// See https://jitsi.github.io/handbook/docs/dev-guide/dev-guide-iframe
declare module "jitsi-meet-external-api" {
  import { TypedEmitter } from "tiny-typed-emitter";

  // https://github.com/jitsi/jitsi-meet/blob/master/config.js
  export interface AppConfig {
    disableInviteFunctions?: boolean;
    doNotStoreRoom?: boolean;
    enableClosePage?: boolean;
    transcribingEnabled?: boolean;
    prejoinPageEnabled?: boolean;
    // ...
  }

  // https://github.com/jitsi/jitsi-meet/blob/master/interface_config.js
  export interface InterfaceConfig {
    TOOLBAR_BUTTONS?: string[];
    SETTINGS_SECTIONS?: string[];
    RECENT_LIST_ENABLED?: boolean;
    // ...
  }

  // This is not well documented but it at least contains these three properties
  export interface UserInfo {
    displayName: string;
    email: string;
    avatarURL: string;
  }

  export interface ApiOptions<InviteeInfo> {
    roomName?: string;
    width?: number | string;
    height?: number | string;
    parentNode?: Element;
    configOverwrite?: AppConfig;
    interfaceConfigOverwrite?: InterfaceConfig;
    noSSL?: boolean;
    jwt?: string;
    onload?: EventHandlerNonNull;
    invitees?: InviteeInfo[];
    devices?: DeviceMap<DeviceLabel>;
    userInfo?: Partial<UserInfo>;
  }

  type RecordingOptions =
    | {
        mode: "stream";
        youtubeStreamKey: string;
        youtubeBroadcastID?: string;
      }
    | {
        mode: "file";
        dropboxToken: string;
      }
    | {
        mode: "file";
        shouldShare?: boolean;
      };

  interface Device {
    deviceId: DeviceId;
    groupId: string;
    kind: DeviceKind;
    label: DeviceLabel;
  }

  type DeviceId = string;
  type DeviceLabel = string;
  type DeviceKind = "audioInput" | "audioOutput" | "videoInput";

  type DeviceMap<T> = {
    [Kind in DeviceKind]: T;
  };

  type ParticipantId = string;

  interface Events {
    cameraError(data: { type: string; message: string }): void;
    avatarChanged(data: { id: ParticipantId; avatarURL: string }): void;
    audioAvailabilityChanged(data: { available: boolean }): void;
    audioMuteStatusChanged(data: { muted: boolean }): void;

    endpointTextMessageReceived(data: {
      senderInfo: {
        jid: string;
        id: ParticipantId;
      };
      eventData: {
        name: "endpoint-text-message";
        text: string;
      };
    }): void;

    micError(data: { type: string; message: string }): void;

    screenSharingStatusChanged(data: {
      on: boolean;
      details: {
        sourceType?: string;
      };
    }): void;

    dominantSpeakerChanged(data: { id: ParticipantId }): void;
    tileViewChanged(data: { enabled: boolean }): void;

    incomingMessage(data: {
      from: ParticipantId;
      nick: string;
      message: string;
    }): void;

    outgoingMessage(data: { message: string }): void;
    displayNameChange(data: { id: ParticipantId; displayname: string }): void;
    deviceListChanged(data: { devices: DeviceMap<Device[]> }): void;
    emailChange(data: { id: ParticipantId; email: string }): void;
    feedbackSubmitted(data: { error: string }): void;
    filmstripDisplayChanged(data: { visible: boolean }): void;
    participantJoined(data: { id: ParticipantId; displayName: string }): void;

    participantKickedOut(data: {
      kicked: {
        id: ParticipantId;
        local: boolean;
      };
      kicker: {
        id: ParticipantId;
      };
    }): void;

    participantLeft(data: { id: ParticipantId }): void;
    participantRoleChanged(data: { id: ParticipantId; role: string }): void;
    passwordRequired(): void;

    videoConferenceJoined(data: {
      roomName: string;
      id: ParticipantId;
      displayName: string;
      avatarURL: string;
    }): void;

    videoConferenceLeft(data: { roomName: string }): void;
    videoAvailabilityChanged(data: { available: boolean }): void;
    videoMuteStatusChanged(data: { muted: boolean }): void;
    readyToClose(): void;
    subjectChange(data: { subject: string }): void;
    suspendDetected(): void;
  }

  interface Commands {
    displayName(newDisplayName: string): void;
    password(password: string): void;
    sendTones(data: { tones: string; duration: number; pause: number }): void;
    subject(newSubject: string): void;

    toggleAudio(): void;
    toggleVideo(): void;
    toggleFilmStrip(): void;
    toggleChat(): void;
    toggleShareScreen(): void;
    toggleTileView(): void;

    hangup(): void;
    email(newEmail: string): void;
    avatarUrl(newAvatarUrl: string): void;
    sendEndpointTextMessage(to: ParticipantId, message: string): void;
    setVideoQuality(verticalResolution: number): void;
    muteEveryone(): void;

    startRecording(options: RecordingOptions): void;
    stopRecording(mode: "stream" | "file"): void;
  }

  export default class JitsiMeetExternalAPI<
    InviteeInfo = never
  > extends TypedEmitter<Events> {
    constructor(domain: string, options: ApiOptions<InviteeInfo>);

    getAvailableDevices(): Promise<DeviceMap<Device[]>>;
    getCurrentDevices(): Promise<DeviceMap<Device>>;

    isDeviceChangeAvailable(): Promise<boolean>;
    isDeviceListAvailable(): Promise<boolean>;
    isMultipleAudioInputSupported(): Promise<boolean>;

    setAudioInputDevice(label: DeviceLabel, deviceId: DeviceId): Promise<void>;
    setAudioOutputDevice(label: DeviceLabel, deviceId: DeviceId): Promise<void>;
    setVideoInputDevice(label: DeviceLabel, deviceId: DeviceId): Promise<void>;

    executeCommand<CommandName extends keyof Commands>(
      command: CommandName,
      ...args: Parameters<Commands[CommandName]>
    ): void;

    executeCommands(
      commands: Partial<
        {
          [CommandName in keyof Commands]: Parameters<Commands[CommandName]>;
        }
      >
    ): void;

    getNumberOfParticipants(): number;

    getAvatarURL(participant: ParticipantId): string;
    getDisplayName(participant: ParticipantId): string;
    getEmail(participant: ParticipantId): string;

    getIFrame(): HTMLIFrameElement;

    isAudioMuted(): Promise<boolean>;
    isVideoMuted(): Promise<boolean>;
    isAudioAvailable(): Promise<boolean>;
    isVideoAvailable(): Promise<boolean>;

    invite(invitees: [object]): Promise<void>;

    dispose(): void;
  }
}
