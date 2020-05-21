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

  getInvitation(code: String) {
    return axios.get(API_URL + `invitation?code=${code}`, {
      headers: authHeader()
    });
  }

  sendInvitations(invitations: Invitation[]) {
    return axios.put(API_URL + "invitation", invitations, {
      headers: authHeader()
    });
  }
}

export default new ApiService();
