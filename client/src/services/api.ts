import {
  Invitation,
  InvitationInsert,
  PresignedURL,
  Room,
  RoomInsert,
  User,
  UserSignUp
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

  // Rooms

  rooms(): Promise<Room[]>;
  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<number[]>;
  joinRoom(roomId: string): Promise<string>;
  leaveRoom(): Promise<void>;

  //  Photos

  preSignURL(code: string): Promise<PresignedURL>;
}

let module: ApiService;
if (process.env.VUE_APP_MOCK_API_SERVICE == "true") {
  module = Mock;
} else {
  module = Server;
}

export default module;
