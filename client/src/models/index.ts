export interface Sync {
  users: UserData[];
  rooms: RoomData[];
  unreadMessages: RecvMessage[];
}

export interface InvitationInsert {
  emailAddress: string;
  firstName: string;
  lastName: string;
  institution: string;
}

export interface Invitation extends InvitationInsert {
  id: number;
  accepted: boolean;
}

export interface UserSignUp {
  invitationCode: string;
  user: {
    emailAddress: string;
    password: string;
    photoURL: string | null;
    displayName: string;
    institution: string;
    website: string;
    bio: string;
    pronouns: string;
  };
}

export interface UserData {
  photoURL: string | null;
  displayName: string;
  institution: string;
  website: string;
  bio: string;
  pronouns: string;
}

export interface User extends UserData {
  id: number;
  level: string;
  room: string | null;
}

export interface RoomData {
  name: string;
  capacity: number;
  color: string;
  topic: string;
  zoomLink: string;
}

export interface Room extends RoomData {
  id: number;
}

export type MessageId = number;

export interface SendMessage {
  senderId: number;
  receiverId: number | null; // null = broadcast
  messageText: string;
  timestamp: number; // time since epoch
}

export interface RecvMessage extends SendMessage {
  messageId: MessageId;
}

export interface MessageModal {
  display: boolean;
  message: string;
  receiverId: number | null; // null = broadcast
}
