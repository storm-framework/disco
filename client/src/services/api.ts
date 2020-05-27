import axios, { AxiosRequestConfig } from "axios";
import _ from "lodash";
import { Invitation, Room, RoomInsert, User } from "../models";

const API_URL = "http://localhost:3000/api";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

class ApiService {
  constructor(private accessToken: string | null) {}

  get sessionUserId(): string | null {
    if (!this.accessToken) {
      return null;
    }
    const payload = _.split(this.accessToken, ".")[1];
    return JSON.parse(atob(payload)).sub;
  }

  // Auth

  async signIn(emailAddress: string, password: string) {
    await delay();
    const response = await axios.post(`${API_URL}/signin`, {
      emailAddress: emailAddress,
      password: password
    });
    if (response.data.accessToken) {
      localStorage.setItem("accessToken", response.data.accessToken);
      this.accessToken = response.data.accessToken;
    }

    return response.data.user;
  }

  async signUp(data: UserSignUp) {
    await delay();
    const response = await axios.put(`${API_URL}/user`, data);
    return response.data;
  }

  signedIn() {
    return this.accessToken !== null;
  }

  signOut() {
    this.accessToken = null;
    localStorage.removeItem("accessToken");
    // TODO: Remove user from room
    return Promise.resolve();
  }

  // Invitations

  getInvitation(code: string): Promise<Invitation> {
    return this.get(`/invitation?code=${code}`);
  }

  sendInvitations(invitations: Invitation[]) {
    return this.put("/invitation", invitations);
  }

  // Users

  users(): Promise<[User]> {
    return this.get("/user");
  }

  // Rooms

  rooms(): Promise<Room[]> {
    return this.get("/room");
  }

  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<[string]> {
    return this.post("/room", {
      inserts: inserts,
      updates: updates
    });
  }

  joinRoom(roomId: string): Promise<string> {
    return this.post(`/room/${roomId}/join`);
  }

  // Request

  async post(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    await delay();
    const response = await axios.post(`${API_URL}${path}`, data, {
      headers: this.authHeader(),
      ...config
    });
    return response.data;
  }

  async get(path: string, config?: AxiosRequestConfig): Promise<any> {
    await delay();
    const response = await axios.get(`${API_URL}${path}`, {
      headers: this.authHeader(),
      ...config
    });
    return response.data;
  }

  async put(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    await delay();
    const response = await axios.put(`${API_URL}${path}`, data, {
      headers: this.authHeader(),
      ...config
    });
    return response.data;
  }

  authHeader() {
    if (this.accessToken) {
      return { Authorization: "Bearer " + this.accessToken };
    } else {
      return {};
    }
  }
}

export interface UserSignUp {
  invitationCode: string;
  user: {
    emailAddress: string;
    password: string;
    fullName: string;
    displayName: string;
    affiliation: string;
  };
}

const accessToken = localStorage.getItem("accessToken");

export default new ApiService(accessToken);
