export interface Invitation {
  emailAddress: string;
  fullName: string;
}

export interface RoomEntity {
  id: string;
  room: Room;
}

export interface Room {
  name: string;
  capacity: number;
  zoomLink: string;
}
