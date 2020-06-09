import {
  Invitation,
  InvitationInsert,
  PresignedURL,
  Room,
  RoomInsert,
  User,
  UserData,
  UserSignUp
} from "@/models";
import axios, { AxiosRequestConfig } from "axios";
import _ from "lodash";

const API_URL = "/api";

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

  async signIn(emailAddress: string, password: string): Promise<User> {
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

  async signUp(data: UserSignUp): Promise<User> {
    await delay();
    const response = await axios.post(`${API_URL}/signup`, data);
    if (response.data.accessToken) {
      localStorage.setItem("accessToken", response.data.accessToken);
      this.accessToken = response.data.accessToken;
    }
    return response.data.user;
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

  getInvitation(param: string): Promise<Invitation> {
    const [id, code] = _.split(param, ".", 2);
    return this.get(`/invitation/${id}?code=${code}`);
  }

  sendInvitations(invitations: InvitationInsert[]) {
    return this.put("/invitation", invitations);
  }

  getInvitations(): Promise<Invitation[]> {
    return this.get("/invitation");
  }

  // Users

  users(): Promise<[User]> {
    return this.get("/user");
  }

  user(userId: number): Promise<User> {
    return this.get(`/user/${userId}`);
  }

  updateUserDataMe(data: UserData): Promise<User> {
    return this.post(`/user/me`, data);
  }

  // Rooms

  rooms(): Promise<Room[]> {
    return this.get("/room");
  }

  updateRooms(updates: Room[], inserts: RoomInsert[]): Promise<number[]> {
    return this.post("/room", {
      inserts: inserts,
      updates: updates
    });
  }

  joinRoom(roomId: string): Promise<string> {
    return this.post(`/room/${roomId}/join`);
  }

  leaveRoom(): Promise<void> {
    return this.post(`/room/leave`);
  }

  // Photos

  preSignURL(code?: string): Promise<PresignedURL> {
    return this.get(`/signurl?code=${code}`);
  }

  // Raw Requests

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

const accessToken = localStorage.getItem("accessToken");

export default new ApiService(accessToken);
