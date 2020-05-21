<template>
  <b-form class="form-signin text-center">
    <h3 class="mb-4">Sign into your account</h3>
    <b-form-input
      id="email-address"
      type="email"
      required
      placeholder="Email address"
    ></b-form-input>

    <b-form-input
      id="password"
      type="password"
      required
      placeholder="Password"
    ></b-form-input>

    <b-button variant="primary" block size="lg" type="submit" class="mt-4"
      >Sign in</b-button
    >
  </b-form>
</template>

<script>
import { Component, Vue } from "vue-property-decorator";

@Component
export default class SignIn extends Vue {}
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
