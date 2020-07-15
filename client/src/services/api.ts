import {
  Invitation,
  InvitationInsert,
  Room,
  RoomData,
  User,
  UserData,
  UserSignUp,
  MessageId,
  RecvMessage,
  SendMessage
} from "@/models";
import Mock from "./api.mock";
import Server from "./api.server";

interface ApiService {
  // Auth
  signIn(emailAddress: string, password: string): Promise<User>;
  signUp(data: UserSignUp): Promise<User>;
  signOut(): Promise<void>;

  // Invitations

  getInvitation(param: string): Promise<Invitation>;
  sendInvitations(invitations: InvitationInsert[]): Promise<string[]>;
  getInvitations(): Promise<Invitation[]>;

  // Users

  users(): Promise<User[]>;
  user(userId: number | "me"): Promise<User>;
  updateUserDataMe(data: UserData): Promise<User>;

  // Rooms

  rooms(): Promise<Room[]>;
  updateRoom(roomId: number, data: RoomData): Promise<Room>;
  updateTopic(roomId: number, topic: string): Promise<Room>;
  updateRooms(updates: Room[], inserts: RoomData[]): Promise<number[]>;
  joinRoom(roomId: string): Promise<string>;
  leaveRoom(): Promise<void>;

  // Upload Files

  uploadFile(file: File, code?: string): Promise<string>;

  // Messages

  recvMessages(): Promise<RecvMessage[]>;
  markRead(msgId: MessageId): Promise<string>;
  sendMessage(msg: SendMessage): Promise<string>;

  // Beacon

  sendBeacon(): void;
}

let module: ApiService;
if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
  module = Mock;
} else {
  module = Server;
}

export default module;
