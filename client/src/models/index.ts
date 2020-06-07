export interface InvitationInsert {
  emailAddress: string;
  firstName: string;
  lastName: string;
  institution: string;
  country: string;
  degree: string;
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

export interface User {
  id: number;
  photoURL: string | null;
  firstName: string;
  lastName: string;
  displayName: string;
  institution: string;
  level: string;
  room: string | null;
}

export interface RoomInsert {
  name: string;
  capacity: number;
  color: string;
  zoomLink: string;
}

export interface Room extends RoomInsert {
  id: number;
}

export interface PresignedURL {
  signedURL: string;
  objectURL: string;
}
