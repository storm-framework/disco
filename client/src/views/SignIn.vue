<template>
  <b-form class="form-signin text-center" @submit="onSubmit">
    <h3 class="mb-4">Sign into your account</h3>
    <b-form-input
      id="email-address"
      type="email"
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

    <b-form-invalid-feedback :state="isValid">
      Incorrect email address or password.
    </b-form-invalid-feedback>

    <b-button
      :disabled="loading"
      variant="primary"
      block
      size="lg"
      type="submit"
      class="mt-4"
    >
      Sign in
    </b-button>
  </b-form>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import ApiService from "../services/api";

@Component
export default class SignIn extends Vue {
  emailAddress = "";
  password = "";
  isValid = true;
  loading = false;

  onSubmit(evt: Event) {
    this.loading = true;
    ApiService.signIn(this.emailAddress, this.password)
      .then(() => {
        this.isValid = true;
        this.$router.replace({ name: "Home" });
      })
      .catch(() => {
        this.loading = false;
        this.isValid = false;
      });
    evt.preventDefault();
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

.form-signin input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
</style>
