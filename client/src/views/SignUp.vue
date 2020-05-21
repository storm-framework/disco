<template>
  <b-form class="form-signup text-center" @submit="onSubmit">
    <h3 class="mb-4">Sign into your account</h3>
    <b-form-group label="Email address" label-for="email-address">
      <b-form-input
        id="email-address"
        type="email"
        v-model="form.emailAddress"
        required
        disabled
      ></b-form-input>
    </b-form-group>

    <b-form-group label="Password" label-for="password">
      <b-form-input
        id="password"
        type="password"
        v-model="form.password"
        required
        placeholder="Password"
      ></b-form-input>
    </b-form-group>

    <b-form-group label="Full name" label-for="full-name">
      <b-form-input
        id="full-name"
        type="text"
        v-model="form.fullName"
        required
        placeholder="Full name"
      ></b-form-input>
    </b-form-group>

    <b-form-group label="Display name" label-for="display-name">
      <b-form-input
        id="display-name"
        type="text"
        v-model="form.fullName"
        required
        placeholder="Display name"
      ></b-form-input>
    </b-form-group>

    <b-form-group label="Affiliation" label-for="affiliation">
      <b-form-input
        id="affiliation"
        type="text"
        v-model="form.fullName"
        placeholder="Affiliation"
      ></b-form-input>
    </b-form-group>

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
      Sign Up
    </b-button>
  </b-form>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import ApiService from "../services/api";

@Component
export default class SignIn extends Vue {
  form = {
    password: "",
    emailAddress: "",
    fullName: "",
    displayName: "",
    affilliation: ""
  };
  loading = false;

  mounted() {
    const code = this.$route.query?.code;
    if (typeof code === "string") {
      ApiService.getInvitation(code).then(console.log);
    }
  }

  onSubmit(evt: Event) {
    this.loading = true;
    // ApiService.signIn(this.emailAddress, this.password)
    //   .then(() => {
    //     this.isValid = true;
    //     this.$router.replace({ name: "Home" });
    //   })
    //   .catch(() => {
    //     this.loading = false;
    //     this.isValid = false;
    //   });
    evt.preventDefault();
  }
}
</script>

<style lang="scss">
.form-signup {
  width: 100%;
  max-width: 350px;
  padding: 15px;
  margin: 0 auto;
}

.form-sigup input[type="email"] {
  margin-bottom: -1px;
  border-bottom-right-radius: 0;
  border-bottom-left-radius: 0;
}

.form-signup .form-control {
  position: relative;
  box-sizing: border-box;
  height: auto;
  padding: 10px;
  font-size: 16px;
}

.form-signup input[type="password"] {
  margin-bottom: 10px;
  border-top-left-radius: 0;
  border-top-right-radius: 0;
}
</style>
