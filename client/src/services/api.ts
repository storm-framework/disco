import { Invitation, InvitationInsert, Room, RoomInsert, User } from "@/models";
import Mock from "./api.mock";
import Server, { UserSignUp } from "./api.server";

interface ApiService {
  sessionUserId: string | null;

  // Auth

  signIn(emailAddress: string, password: string): Promise<User>;

  signUp(data: UserSignUp): Promise<{ id: string }>;

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

  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<string[]>;

  joinRoom(roomId: string): Promise<string>;
}

let module: ApiService;
if (process.env.VUE_APP_MOCK_SERVICE == "true") {
  console.log("here");
  module = Mock;
} else {
  module = Server;
}

export default module;
