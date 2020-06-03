<template>
  <b-overlay
    :show="loading"
    spinner-variant="primary"
    spinner-type="grow"
    spinner-small
    rounded="sm"
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

      <b-form-group label="First name" label-for="full-name">
        <b-form-input
          id="first-name"
          type="text"
          v-model="form.firstName"
          required
          placeholder="First name"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Last name" label-for="full-name">
        <b-form-input
          id="last-name"
          type="text"
          v-model="form.lastName"
          required
          placeholder="Last name"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Badge name" label-for="display-name">
        <b-form-input
          id="display-name"
          type="text"
          v-model="form.displayName"
          required
          placeholder="Badge name"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Institution" label-for="institution">
        <b-form-input
          id="institution"
          type="text"
          v-model="form.institution"
          placeholder="Institution"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Country" label-for="country">
        <b-form-input
          id="country"
          type="text"
          v-model="form.institution"
          placeholder="Country"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

      <b-form-group label="Degree" label-for="degree">
        <b-form-input
          id="degree"
          type="text"
          v-model="form.institution"
          placeholder="Degree"
          :disabled="fatalError"
        ></b-form-input>
      </b-form-group>

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
import _ from "lodash";

@Component
export default class SignIn extends Vue {
  form = {
    password: "",
    emailAddress: "",
    firstName: "",
    lastName: "",
    displayName: "",
    institution: "",
    country: "",
    degree: ""
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
          const displayName = _([invitation.firstName, invitation.lastName])
            .filter()
            .join(" ");
          this.form = { password: "", displayName, ...invitation };
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
    this.$store
      .dispatch("signUp", { invitationCode: code, user: this.form })
      .then(() => this.$router.replace({ name: "Home" }))
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
