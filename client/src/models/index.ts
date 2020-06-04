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
    country: string;
    degree: string;
  };
}

export interface User {
  id: number;
  displayName: string;
  level: string;
  room: string | null;
}

export interface RoomInsert {
  color: string;
  name: string;
  capacity: number;
  zoomLink: string;
}

export interface Room extends RoomInsert {
  id: number;
}

export interface PresignedURL {
  signedURL: string;
  objectURL: string;
}
