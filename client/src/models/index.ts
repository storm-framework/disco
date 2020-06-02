export interface Invitation {
  id: string;
  emailAddress: string;
  fullName: string;
  accepted: boolean;
}

export interface InvitationInsert {
  emailAddress: string;
  fullName: string;
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
