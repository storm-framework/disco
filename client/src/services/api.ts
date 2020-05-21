import axios from "axios";

const API_URL = "http://localhost:3000/";

class ApiService {
  async signIn(emailAddress: string, password: string) {
    const response = await axios.post(API_URL + "sign_in", {
      emailAddress: emailAddress,
      password: password,
    });
    if (response.data.accessToken) {
      localStorage.setItem("accessToken", response.data.accessToken);
    }

    return response.data.user;
  }

  logOut() {
    localStorage.removeItem("accessToken");
  }
}

export default new ApiService();
