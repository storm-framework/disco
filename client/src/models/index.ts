export interface InvitationInsert {
  emailAddress: string;
  firstName: string;
  lastName: string;
  institution: string;
  country: string;
  degree: string;
}

export interface Invitation extends InvitationInsert {
  id: string;
  accepted: boolean;
}

export interface UserSignUp {
  invitationCode: string;
  user: {
    emailAddress: string;
    password: string;
    firstName: string;
    displayName: string;
    institution: string;
    country: string;
    degree: string;
  };
}

export interface User {
  id: string;
  displayName: string;
  level: string;
  room: string | null;
}

export interface RoomInsert {
  name: string;
  capacity: number;
  zoomLink: string;
}

export interface Room extends RoomInsert {
  id: string;
}
