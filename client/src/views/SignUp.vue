<template>
  <b-overlay
    :show="loading"
    spinner-variant="primary"
    spinner-type="grow"
    spinner-small
    rounded="sm"
    style="max-width: 320px;"
    class="form-signup"
    bg-color="#f5f5f5"
  >
    <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
    <b-form class="text-center" @submit="onSubmit">
      <h3 class="mb-4">Sign up</h3>
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
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Full name" label-for="full-name">
        <b-form-input
          id="full-name"
          type="text"
          v-model="form.fullName"
          required
          placeholder="Full name"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Display name" label-for="display-name">
        <b-form-input
          id="display-name"
          type="text"
          v-model="form.displayName"
          required
          placeholder="Display name"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Affiliation" label-for="affiliation">
        <b-form-input
          id="affiliation"
          type="text"
          v-model="form.affiliation"
          placeholder="Affiliation"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <!-- <b-form-invalid-feedback :state="isValid">
      Incorrect email address or password.
    </b-form-invalid-feedback> -->

      <b-button
        :disabled="fatalError"
        variant="primary"
        block
        size="lg"
        type="submit"
        class="mt-4"
      >
        Sign Up
      </b-button>
    </b-form>
  </b-overlay>
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
    affiliation: ""
  };
  loading = false;
  fatalError = false;
  errorMsg = "";

  mounted() {
    const code = this.$route.query?.code;
    if (typeof code === "string") {
      this.loading = true;
      ApiService.getInvitation(code)
        .then(invitation => {
          this.form.emailAddress = invitation.emailAddress;
          this.form.fullName = invitation.fullName;
          this.loading = false;
        })
        .catch(error => {
          if (error.response?.status == 404) {
            this.setFatalError("Invalid invitation code");
          } else {
            this.setFatalError("Internal server error");
          }
          this.loading = false;
        });
    } else {
      this.setFatalError("Missing invitation code");
    }
  }

  setFatalError(msg: string) {
    this.errorMsg = msg;
    this.fatalError = true;
  }

  onSubmit(evt: Event) {
    const code = this.$route.query?.code;
    if (typeof code !== "string") {
      return;
    }
    ApiService.signUp({ invitationCode: code, user: this.form })
      .then(console.log)
      .catch(error => {
        if (error.response?.status == 403) {
          this.setFatalError("Invalid invitation code");
        } else {
          this.setFatalError("Internal server error");
        }
        this.loading = false;
      });
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
</style>
