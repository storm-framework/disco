export interface Invitation {
  emailAddress: string;
  fullName: string;
}

export interface User {
  id: string;
  displyName: string;
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
