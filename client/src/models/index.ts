export interface Invitation {
  emailAddress: string;
  fullName: string;
}

export interface Entity<T> {
  id: string;
  data: T;
}

export interface User {
  displyName: string;
}

export interface Room {
  name: string;
  capacity: number;
  zoomLink: string;
  users?: User[];
}
