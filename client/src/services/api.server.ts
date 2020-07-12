import {
  Invitation,
  InvitationInsert,
  Room,
  RoomData,
  User,
  UserData,
  UserSignUp,
  SendMessage,
  MessageId
} from "@/models";
import router from "@/router";
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
  constructor(private accessToken: string | null) { }
  
  readUpto: MessageId = 0;

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

  updateRoom(id: number, data: RoomData): Promise<Room> {
    return this.post(`/room/${id}`, data);
  }

  updateTopic(id: number, topic: string): Promise<Room> {
    return this.post(`/room/${id}/topic`, topic);
  }

  updateRooms(updates: Room[], inserts: RoomData[]): Promise<number[]> {
    return this.post("/room", {
      inserts: inserts,
      updates: updates
    });
  }

  joinRoom(roomId: string): Promise<string> {
    return this.post(`/room/${roomId}/join`);
  }

  leaveRoom(): Promise<void> {
    return this.post(`/room/current/leave`);
  }

  // Files

  presignURL(param?: string): Promise<string> {
    if (param) {
      const [id, code] = _.split(param, ".");
      return axios
        .get(`${API_URL}/signurl?code=${code}&id=${id}`)
        .then(r => r.data);
    } else {
      return this.get("/signurl");
    }
  }

  async uploadFile(file: File, code?: string): Promise<string> {
    const presigned = await this.presignURL(code);
    await axios.put(presigned, file, {
      headers: { "Content-Type": file.type }
    });
    const u = new URL(presigned);
    return `${u.protocol}//${u.host}${u.pathname}`;
  }

  // Messages

  messages(): Promise<SendMessage[]> {
    // the "from:MessageId" should be obtained from local-storage
    alert("FIXME:api.server.getMessages");
    return Promise.resolve([]);
  }

  markRead(msgId: MessageId): Promise<void> {
    const readToStr = localStorage.getItem("readUpto");
    var readTo = 0;
    if (readToStr) { 
      readTo = +readToStr;
    }
    if (readTo < msgId) {
      localStorage.setItem("readUpto", msgId.toString());
    }
    return Promise.resolve();
  }


  // Raw Requests

  async post(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    await delay();
    try {
      const response = await axios.post(`${API_URL}${path}`, data, {
        headers: this.authHeader(),
        ...config
      });
      axios.create({});
      return response.data;
    } catch (error) {
      if (error?.response?.status == 401) {
        await this.unauthorized();
      }
      throw error;
    }
  }

  async get(path: string, config?: AxiosRequestConfig): Promise<any> {
    await delay();
    try {
      const response = await axios.get(`${API_URL}${path}`, {
        headers: this.authHeader(),
        ...config
      });
      return response.data;
    } catch (error) {
      if (error?.response?.status == 401) {
        await this.unauthorized();
      }
      throw error;
    }
  }

  async put(
    path: string,
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<any> {
    await delay();
    try {
      const response = await axios.put(`${API_URL}${path}`, data, {
        headers: this.authHeader(),
        ...config
      });
      return response.data;
    } catch (error) {
      if (error?.response?.status == 401) {
        await this.unauthorized();
      }
      throw error;
    }
  }

  authHeader() {
    if (this.accessToken) {
      return { Authorization: "Bearer " + this.accessToken };
    } else {
      return {};
    }
  }

  async unauthorized() {
    await this.signOut();
    router.replace({ name: "signIn" });
  }
}

const accessToken = localStorage.getItem("accessToken");

export default new ApiService(accessToken);
