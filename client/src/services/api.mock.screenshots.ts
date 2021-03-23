import {
  Invitation,
  InvitationInsert,
  MessageId,
  RecvMessage,
  Room,
  RoomData,
  SendMessage,
  Sync,
  User,
  UserData,
  UserSignUp
} from "@/models";
import _ from "lodash";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

const NOW = new Date().getTime();

const MESSAGES: RecvMessage[] = [];

const INVITATION: Invitation = {
  id: 1,
  emailAddress: "charlie@hba.org",
  firstName: "Charlie",
  lastName: "Papazian",
  institution: "Home Brewers Association",
  accepted: false
};

const ROOMS: { [id: string]: Room } = {
  1: {
    id: 1,
    name: "Green Room",
    capacity: 8,
    zoomLink: "https://meet.jit.si/109283740293847",
    color: "#00ff00",
    topic: "Writers"
  },
  2: {
    id: 2,
    name: "Red Room",
    capacity: 5,
    zoomLink: "https://meet.jit.si/018471092384710",
    color: "#ff0000",
    topic: "Russian River"
  },
  3: {
    id: 3,
    name: "Blue Room",
    capacity: 10,
    zoomLink: "https://meet.jit.si/102389471203487",
    color: "#0000ff",
    topic: "Stone"
  }
};

const USERS: { [id: string]: User } = {
  1: {
    id: 1,
    photoURL:
      "https://s3-us-west-2.amazonaws.com/brewersassoc/wp-content/uploads/2019/01/cp-feat-2-600x454.jpg",
    displayName: "Charlie Papazian",
    institution: "Homebrewers Association",
    pronouns: "he/him",
    bio:
      "American nuclear engineer, brewer and author. Founder of the Association of Brewers, American Homebrewers Association and the Great American Beer Festival",
    website: "",
    level: "attendee",
    room: null,
    isActive: true
  },
  2: {
    id: 2,
    photoURL:
      "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fmovimentobirra.files.wordpress.com%2F2013%2F05%2F03-papaz-filtered.jpg&f=1&nofb=1",
    displayName: "Tom Acitelli",
    institution: "Curbed Boston",
    pronouns: "he/him",
    bio: "",
    website: "",
    level: "attendee",
    room: "1",
    isActive: true
  },
  3: {
    id: 3,
    photoURL:
      "https://upload.wikimedia.org/wikipedia/commons/2/2a/Michael_jackson_beer.jpg",
    displayName: "Michael Jackson",
    institution:
      "Hello, my name is Michael Jackson. No, not that Michael Jackson, but I am on a world tour. My tour is in pursuit of exceptional beer. That's why they call me the Beer Hunter.",
    pronouns: "he/him",
    bio: "",
    website: "",
    level: "attendee",
    room: null,
    isActive: true
  },
  4: {
    id: 4,
    photoURL:
      "https://upload.wikimedia.org/wikipedia/commons/thumb/2/22/Vinnie_Cilurzo.jpg/220px-Vinnie_Cilurzo.jpg",
    displayName: "Vinnie Cilurzo",
    institution: "Russian River Brewing Company",
    pronouns: "he/him",
    bio: "",
    website: "",
    level: "attendee",
    room: "2",
    isActive: true
  },
  5: {
    id: 5,
    photoURL:
      "https://external-content.duckduckgo.com/iu/?u=http%3A%2F%2Fwww.californiacraftbeer.com%2Fwp-content%2Fuploads%2F2015%2F03%2FNatalie-Cilurzo-RR.jpg&f=1&nofb=1",
    displayName: "Natalie Cilurzo",
    institution: "Russian River Brewery",
    pronouns: "she/her",
    bio: "",
    website: "",
    level: "attendee",
    room: "2",
    isActive: true
  },
  6: {
    id: 6,
    photoURL:
      "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.izIaTwMlr-QgFNPtsRp-pwHaFj%26pid%3DApi&f=1",
    displayName: "Greg Koch",
    institution: "Stone Brewing",
    pronouns: "he/him",
    bio: "",
    website: "",
    level: "attendee",
    room: "3",
    isActive: true
  },
  7: {
    id: 7,
    photoURL:
      "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fi.ytimg.com%2Fvi%2FSrt_pRexFh8%2Fmaxresdefault.jpg&f=1&nofb=1",
    displayName: "Steve Wagner",
    institution: "Stone Brewing",
    pronouns: "he/him",
    bio: "",
    website: "",
    level: "attendee",
    room: "3",
    isActive: true
  }
};

const SESSION_USER_ID = "3";

/* eslint-disable @typescript-eslint/no-unused-vars */
class ApiService {
  readUpto: MessageId = 0;

  currentClock = 0;

  // Auth

  async signIn(emailAddress: string, password: string): Promise<User> {
    await delay();
    return USERS[SESSION_USER_ID];
  }

  async signUp(data: UserSignUp): Promise<User> {
    await delay();
    return USERS[SESSION_USER_ID];
  }

  signOut() {
    return Promise.resolve();
  }

  // Invitations

  async getInvitation(param: string): Promise<Invitation> {
    await delay();
    return INVITATION;
  }

  sendInvitations(invitations: InvitationInsert[]): Promise<string[]> {
    return Promise.reject("Not implemented");
  }

  getInvitations(): Promise<Invitation[]> {
    return Promise.reject("Not implemented");
  }

  // Users

  async user(userIdOrMe: number | "me"): Promise<User> {
    await delay();
    const userId = userIdOrMe == "me" ? SESSION_USER_ID : userIdOrMe;
    const user = USERS[userId];
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

  updateRoom(id: number, data: RoomData): Promise<Room> {
    return Promise.resolve({ ...data, id });
  }

  updateTopic(id: number, topic: string): Promise<Room> {
    return Promise.resolve({ ...ROOMS[id], topic });
  }

  updateRooms(updates: Room[], inserts: RoomData[]): Promise<number[]> {
    return Promise.reject("Not implemented");
  }

  joinRoom(roomId: number): Promise<void> {
    return Promise.resolve();
  }

  joinRandom(): Promise<number> {
    const random = _.sample(Object.keys(ROOMS)) || "1";
    return Promise.resolve(parseInt(random));
  }

  leaveRoom(): Promise<void> {
    return Promise.reject("Not implemented");
  }

  // Files

  async uploadFile(file: File, code?: string): Promise<string> {
    await delay();
    return Promise.resolve(
      "https://upload.wikimedia.org/wikipedia/commons/thumb/7/77/Avatar_cat.png/120px-Avatar_cat.png"
    );
  }

  // Messages

  getUnreadMessages(currentClock: number): RecvMessage[] {
    return MESSAGES.filter(
      v => this.readUpto < v.messageId && v.messageId <= currentClock
    );
  }

  recvMessages(): Promise<RecvMessage[]> {
    return Promise.resolve(this.getUnreadMessages(this.currentClock++));
  }

  markRead(msgId: MessageId): Promise<string> {
    const alreadyReadUpto = this.readUpto;
    if (alreadyReadUpto < msgId) {
      this.readUpto = msgId;
    }
    return Promise.resolve("ok");
  }

  sendMessage(sendMsg: SendMessage): Promise<string> {
    const recvMsg: RecvMessage = Object.assign(sendMsg, {
      messageId: this.currentClock
    });
    MESSAGES.push(recvMsg);
    // console.log("Sending a message:", recvMsg);
    return Promise.resolve("ok");
  }

  // Sync

  async sync(): Promise<Sync> {
    await delay();
    const msg = (this.currentClock += 1);
    return {
      rooms: Object.values(ROOMS),
      users: Object.values(USERS),
      unreadMessages: this.getUnreadMessages(this.currentClock++)
    };
  }

  sendBeacon() {
    return;
  }

  // Errors

  errorResponse(status: number): Promise<any> {
    return Promise.reject({ response: { status: status } });
  }
}

export default new ApiService();
