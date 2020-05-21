<template>
  <b-form class="form-signin text-center" @submit="onSubmit" @reset="onReset">
    <h3 class="mb-4">Sign into your account</h3>
    <b-form-input
      id="email-address"
      type="text"
      v-model="emailAddress"
      required
      placeholder="Email address"
    ></b-form-input>

    <b-form-input
      id="password"
      type="password"
      v-model="password"
      required
      placeholder="Password"
    ></b-form-input>

    <b-button variant="primary" block size="lg" type="submit" class="mt-4"
      >Sign in</b-button
    >
  </b-form>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import ApiService from "../services/api";

@Component
export default class SignIn extends Vue {
  emailAddress = "";
  password = "";

  onSubmit(evt: Event) {
    ApiService.signIn(this.emailAddress, this.password).then((response) => {
      console.log(response);
    });
    evt.preventDefault();
  }

  onReset() {
    console.log("here");
  }
}
</script>

<style lang="scss">
.form-signin {
  width: 100%;
  max-width: 350px;
  padding: 15px;
  margin: 0 auto;
}

.form-signin input[type="email"] {
  margin-bottom: -1px;
  border-bottom-right-radius: 0;
  border-bottom-left-radius: 0;
}

.form-signin .form-control {
  position: relative;
  box-sizing: border-box;
  height: auto;
  padding: 10px;
  font-size: 16px;
}

.form-signin input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
</style>
