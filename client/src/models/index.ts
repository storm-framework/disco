export interface Invitation {
  emailAddress: string;
  fullName: string;
}

export interface Entity<T> {
  id: string;
  data: T;
}

export interface User {
  id: string;
  displyName: string;
  room: string | null;
}

export interface Room {
  name: string;
  capacity: number;
  zoomLink: string;
}
