import {
  Invitation,
  InvitationInsert,
  Room,
  RoomInsert,
  User,
  UserSignUp
} from "@/models";

const API_URL = "/api";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

const ROOMS: { [id: string]: Room } = {
  1: {
    id: "1",
    name: "Green Room",
    capacity: 8,
    zoomLink: "https://meet.jitsi.si/109283740293847"
  },
  2: {
    id: "2",
    name: "Red Room",
    capacity: 5,
    zoomLink: "https://meet.jitsi.si/018471092384710"
  },
  3: {
    id: "3",
    name: "Pink Room",
    capacity: 10,
    zoomLink: "https://meet.jitsi.si/102389471203487"
  }
};

const USERS: { [id: string]: User } = {
  1: {
    id: "1",
    displayName: "Charlie Papazian",
    level: "organizer",
    room: "1"
  },
  2: {
    id: "2",
    displayName: "Michael Jackson",
    level: "atendee",
    room: "1"
  },
  3: {
    id: "3",
    displayName: "Natalie Cilurzo",
    level: "attendee",
    room: "2"
  },
  4: {
    id: "4",
    displayName: "Vinnie Cilurzo",
    level: "atendee",
    room: "2"
  },
  5: {
    id: "5",
    displayName: "Greg Koch",
    level: "atendee",
    room: "3"
  },
  6: {
    id: "6",
    displayName: "Dominic Engels",
    level: "atendee",
    room: "3"
  },
  7: {
    id: "7",
    displayName: "Steve Wagner",
    level: "atendee",
    room: "3"
  }
};

const SESSION_USER_ID = "1";

class ApiService {
  constructor(private accessToken: string | null) {}

  get sessionUserId(): string | null {
    if (!this.accessToken) {
      return null;
    }
    return SESSION_USER_ID;
  }

  // Auth

  async signIn(emailAddress: string, password: string) {
    await delay();
    this.accessToken = "accessToken";
    return USERS[SESSION_USER_ID];
  }

  async signUp(data: UserSignUp): Promise<{ id: string }> {
    await delay();
    return { id: SESSION_USER_ID };
  }

  signedIn() {
    return this.accessToken !== null;
  }

  signOut() {
    this.accessToken = null;
    return Promise.resolve();
  }

  // Invitations

  getInvitation(param: string): Promise<Invitation> {
    return Promise.reject("Not implemented");
  }

  sendInvitations(invitations: InvitationInsert[]): Promise<string[]> {
    return Promise.reject("Not implemented");
  }

  getInvitations(): Promise<Invitation[]> {
    return Promise.reject("Not implemented");
  }

  // Users

  async users(): Promise<User[]> {
    await delay();
    return Object.values(USERS);
  }

  // Rooms

  rooms(): Promise<Room[]> {
    return Promise.resolve(Object.values(ROOMS));
  }

  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<string[]> {
    return Promise.reject("Not implemented");
  }

  joinRoom(roomId: string): Promise<string> {
    return Promise.resolve(ROOMS[roomId].zoomLink);
  }

  leaveRoom(): Promise<void> {
    return Promise.reject("Not implemented");
  }
}

export default new ApiService("accessToken");
