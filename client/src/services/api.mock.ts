import {
  Invitation,
  InvitationInsert,
  PresignedURL,
  Room,
  RoomInsert,
  User,
  UserData,
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
    id: 1,
    name: "Green Room",
    capacity: 8,
    zoomLink: "https://meet.jitsi.si/109283740293847",
    color: "#00ff00"
  },
  2: {
    id: 2,
    name: "Red Room",
    capacity: 5,
    zoomLink: "https://meet.jitsi.si/018471092384710",
    color: "#ff0000"
  },
  3: {
    id: 3,
    name: "Blue Room",
    capacity: 10,
    zoomLink: "https://meet.jitsi.si/102389471203487",
    color: "#0000ff"
  }
};

const USERS: { [id: string]: User } = {
  1: {
    id: 1,
    photoURL: null,
    firstName: "Charlie",
    lastName: "Papazian",
    displayName: "Charlie",
    institution: "Homebrewers",
    level: "organizer",
    room: "1"
  },
  2: {
    id: 2,
    firstName: "Michael",
    lastName: "Jackson",
    photoURL: null,
    displayName: "Beerhunter",
    institution: "Beer",
    level: "atendee",
    room: "1"
  },
  3: {
    id: 3,
    firstName: "Natalie",
    lastName: "Cilurzo",
    photoURL: null,
    displayName: "Natalie",
    institution: "Russian River",
    level: "attendee",
    room: "2"
  },
  4: {
    id: 4,
    photoURL: null,
    firstName: "Vinnie",
    lastName: "Cilurzo",
    displayName: "Vinnie",
    institution: "Russina River",
    level: "atendee",
    room: "2"
  },
  5: {
    id: 5,
    photoURL: null,
    firstName: "Greg",
    lastName: "Koch",
    displayName: "Greg Koch",
    institution: "Stone Brewing",
    level: "atendee",
    room: "3"
  },
  6: {
    id: 6,
    photoURL: null,
    firstName: "Dominic",
    lastName: "Brewing",
    displayName: "Dominic",
    institution: "Stone Brewing",
    level: "atendee",
    room: "3"
  },
  7: {
    id: 7,
    photoURL: null,
    firstName: "Steve",
    lastName: "Wagner",
    displayName: "Steve",
    institution: "Stone Brewing",
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

  async signIn(emailAddress: string, password: string): Promise<User> {
    await delay();
    this.accessToken = "accessToken";
    return USERS[SESSION_USER_ID];
  }

  async signUp(data: UserSignUp): Promise<User> {
    await delay();
    return USERS[SESSION_USER_ID];
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

  async user(userId: number): Promise<User> {
    await delay();
    const user = USERS[SESSION_USER_ID];
    if (user) {
      return user;
    } else {
      return this.errorResponse(404);
    }
  }

  async users(): Promise<User[]> {
    await delay();
    return Object.values(USERS);
  }

  async updateUserDataMe(data: UserData): Promise<User> {
    await delay();
    const user = USERS[SESSION_USER_ID];
    if (user) {
      return { ...user, ...data };
    } else {
      return this.errorResponse(404);
    }
  }

  // Rooms

  rooms(): Promise<Room[]> {
    return Promise.resolve(Object.values(ROOMS));
  }

  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<number[]> {
    return Promise.reject("Not implemented");
  }

  joinRoom(roomId: string): Promise<string> {
    return Promise.resolve(ROOMS[roomId].zoomLink);
  }

  leaveRoom(): Promise<void> {
    return Promise.reject("Not implemented");
  }

  // Photos

  preSignURL(code?: string): Promise<PresignedURL> {
    return Promise.reject("Not implemented");
  }

  // Errors

  errorResponse(status: number): Promise<any> {
    return Promise.reject({ response: { status: status } });
  }
}

export default new ApiService("accessToken");
