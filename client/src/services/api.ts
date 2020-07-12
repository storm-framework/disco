import {
  Invitation,
  InvitationInsert,
  Room,
  RoomData,
  User,
  UserData,
  UserSignUp,
  MessageId,
  SendMessage
} from "@/models";
import Mock from "./api.mock";
import Server from "./api.server";

interface ApiService {
  sessionUserId: string | null;

  // Auth

  signIn(emailAddress: string, password: string): Promise<User>;
  signUp(data: UserSignUp): Promise<User>;
  signedIn(): boolean;
  signOut(): Promise<void>;

  // Invitations

  getInvitation(param: string): Promise<Invitation>;
  sendInvitations(invitations: InvitationInsert[]): Promise<string[]>;
  getInvitations(): Promise<Invitation[]>;

  // Users

  users(): Promise<User[]>;
  user(userId: number): Promise<User>;
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
  messages(): Promise<SendMessage[]>;
  markRead(msgId: MessageId): Promise<void>;
}

let module: ApiService;
if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
  module = Mock;
} else {
  module = Server;
}

export default module;
