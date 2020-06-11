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
              v-model="emailAddress"
              required
              disabled
            ></b-form-input>
          </b-form-group>

          <b-form-group label="Password*" label-for="password">
            <b-form-input
              id="password"
              type="password"
              v-model="password"
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
          <profile-form :disabled="fatalError" v-model="profile" />
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
import ProfileForm, { ProfileFormData } from "@/components/ProfileForm.vue";

interface Form {
  emailAddress: string;
  password: string;
}

@Component({ components: { ProfileForm } })
export default class SignIn extends Vue {
  emailAddress = "";
  password = "";
  profile: ProfileFormData = {
    displayName: "",
    photo: {
      previewURL: null,
      file: null
    },
    pronouns: "",
    institution: "",
    website: "",
    bio: ""
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
          this.emailAddress = invitation.emailAddress;
          this.password = "";
          this.profile = {
            ...this.profile,
            displayName,
            institution: invitation.institution
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
    if (this.profile.photo.file) {
      photoURL = await ApiService.uploadFile(this.profile.photo.file, code);
    }
    const data: UserSignUp = {
      invitationCode: code,
      user: {
        emailAddress: this.emailAddress,
        password: this.password,
        ...this.profile,
        photoURL
      }
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
