import axios from "axios";
import { Invitation } from "../models";

const API_URL = "http://localhost:3000/api/";

function authHeader() {
  const accessToken = localStorage.getItem("accessToken");

  if (accessToken) {
    return { Authorization: "Bearer " + accessToken };
  } else {
    return {};
  }
}

function wait(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

class ApiService {
  async signIn(emailAddress: string, password: string) {
    const response = await axios.post(API_URL + "signin", {
      emailAddress: emailAddress,
      password: password
    });
    if (response.data.accessToken) {
      localStorage.setItem("accessToken", response.data.accessToken);
    }

    return response.data.user;
  }

  logOut() {
    localStorage.removeItem("accessToken");
  }

  async getInvitation(code: string): Promise<Invitation> {
    await wait();
    const response = await axios.get(API_URL + `invitation?code=${code}`, {
      headers: authHeader()
    });
    return response.data;
  }

  async sendInvitations(invitations: Invitation[]) {
    await wait();
    const response = await axios.put(API_URL + "invitation", invitations, {
      headers: authHeader()
    });
    return response.data;
  }

  public async signUp(data: UserSignUp) {
    await wait();
    const response = await axios.put(API_URL + "user", data, {
      headers: authHeader()
    });
    return response.data;
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

export default new ApiService();
