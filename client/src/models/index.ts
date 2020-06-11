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
    firstName: string;
    lastName: string;
    photoURL: string | null;
    displayName: string;
    institution: string;
  };
}

export interface UserData {
  firstName: string;
  lastName: string;
  photoURL: string | null;
  displayName: string;
  institution: string;
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
