import axios from "axios";

const API_URL = "http://localhost:3000/";

class ApiService {
  signIn(username: string, password: string) {
    return axios.post(API_URL + "sign_in", {
      username: username,
      password: password,
    });
  }
}

export default new ApiService();
