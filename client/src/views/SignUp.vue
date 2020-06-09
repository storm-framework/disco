<template>
  <b-overlay
    :show="loading || sending"
    spinner-variant="primary"
    spinner-type="grow"
    spinner-small
    rounded="sm"
    class="form-signup"
    bg-color="#f5f5f5"
  >
    <b-container>
      <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
      <b-form @submit.prevent="onSubmit">
        <b-form-group
          label-cols-lg="3"
          label="Sign In"
          label-size="lg"
          label-class="font-weight-bold pt-0"
          class="mb-5"
        >
          <b-form-group label="Email address*" label-for="email-address">
            <b-form-input
              id="email-address"
              type="email"
              v-model="form.emailAddress"
              required
              disabled
            ></b-form-input>
          </b-form-group>

          <b-form-group
            label="Password*"
            label-for="password"
            description="Choose an easy password. This is just a demo and your account will be deleted afterwards."
          >
            <b-form-input
              id="password"
              type="password"
              v-model="form.password"
              required
              placeholder="Password"
              :disabled="fatalError"
            ></b-form-input>
          </b-form-group>
        </b-form-group>

        <b-form-group
          label-cols-lg="3"
          label="Profile"
          label-size="lg"
          label-class="font-weight-bold pt-0"
          class="mb-0"
        >
          <b-form-group label="First name*" label-for="full-name">
            <b-form-input
              id="first-name"
              type="text"
              v-model="form.firstName"
              required
              placeholder="First name"
              :disabled="fatalError"
            ></b-form-input>
          </b-form-group>

          <b-form-group label="Last name*" label-for="full-name">
            <b-form-input
              id="last-name"
              type="text"
              v-model="form.lastName"
              required
              placeholder="Last name"
              :disabled="fatalError"
            ></b-form-input>
          </b-form-group>

          <b-form-group
            label="Badge name*"
            label-for="display-name"
            description="People will use your badge name to identify you in the app."
          >
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

          <b-form-group label="Photo" label-for="photo">
            <photo-input
              @fileInput="form.photoFile = $event"
              :disabled="fatalError"
            />
          </b-form-group>
        </b-form-group>

        <b-form-group label-cols-lg="3">
          <b-button
            :disabled="fatalError"
            variant="primary"
            size="lg"
            type="submit"
            class="mt-4"
          >
            Sign up
          </b-button>
        </b-form-group>
      </b-form>
    </b-container>
  </b-overlay>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import ApiService from "@/services/api";
import _ from "lodash";
import { UserSignUp } from "@/models";
import PhotoInput from "@/components/PhotoInput.vue";

interface Form {
  password: string;
  emailAddress: string;
  firstName: string;
  lastName: string;
  displayName: string;
  institution: string;
  photoFile: File | null;
}

@Component({ components: { PhotoInput } })
export default class SignIn extends Vue {
  form: Form = {
    password: "",
    emailAddress: "",
    firstName: "",
    lastName: "",
    displayName: "",
    institution: "",
    photoFile: null
  };
  loading = false;
  fatalError = false;
  errorMsg = "";
  sending = false;

  mounted() {
    const code = this.code();
    if (typeof code === "string") {
      this.loading = true;
      ApiService.getInvitation(code)
        .then(invitation => {
          const displayName = _([invitation.firstName, invitation.lastName])
            .filter()
            .join(" ");
          this.form = {
            password: "",
            photoFile: null,
            displayName,
            ...invitation
          };
        })
        .catch(error => {
          if (error.response?.status == 404) {
            this.setFatalError("Invalid invitation code");
          } else {
            this.setFatalError("Internal server error");
          }
        })
        .finally(() => (this.loading = false));
    } else {
      this.setFatalError("Missing invitation code");
    }
  }

  setFatalError(msg: string) {
    this.errorMsg = msg;
    this.fatalError = true;
  }

  onSubmit() {
    const code = this.code();
    if (this.sending || !code) {
      return;
    }
    this.sending = true;
    this.submit(code)
      .catch(error => {
        if (error.response?.status == 403) {
          this.setFatalError("Invalid invitation code");
        } else {
          this.setFatalError("Internal server error");
        }
      })
      .finally(() => {
        this.sending = false;
      });
  }

  async submit(code: string) {
    let photoURL = null;
    if (this.form.photoFile) {
      photoURL = await ApiService.uploadFile(this.form.photoFile, code);
    }
    const data: UserSignUp = {
      invitationCode: code,
      user: { ...this.form, photoURL }
    };
    await this.$store.dispatch("signUp", data);
    this.$router.replace({ name: "Home" });
  }

  code(): string | null {
    const c = this.$route.query?.code;
    if (typeof c === "string") {
      return c;
    } else {
      return null;
    }
  }
}
</script>

<style lang="scss">
.form-signup {
  width: 100%;
  padding: 15px;
  margin: 0 auto;
}
</style>
