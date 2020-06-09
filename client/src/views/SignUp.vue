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
            <b-form-row>
              <b-col>
                <b-form-file
                  placeholder="Choose a file or drop it here"
                  drop-placeholder="Drop file here..."
                  v-model="form.photo"
                  accept="image/*"
                  v-on:input="onPhotoChange"
                  :disabled="fatalError"
                >
                </b-form-file>
              </b-col>
              <b-col cols="auto" v-if="photoSrc">
                <b-avatar :src="photoSrc" />
              </b-col>
              <modal-cropper ref="cropper" v-on:ok="onAcceptImage" />
            </b-form-row>
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
import axios from "axios";
import { UserSignUp } from "@/models";
import ModalCropper from "@/components/ModalCropper.vue";

interface Form {
  password: string;
  emailAddress: string;
  firstName: string;
  lastName: string;
  displayName: string;
  institution: string;
}

@Component({ components: { ModalCropper } })
export default class SignIn extends Vue {
  form: Form = {
    password: "",
    emailAddress: "",
    firstName: "",
    lastName: "",
    displayName: "",
    institution: ""
  };
  photoFile: File | null = null;
  photoSrc: string | null = null;
  loading = false;
  fatalError = false;
  errorMsg = "";
  sending = false;

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

  onSubmit() {
    if (this.sending) {
      return;
    }
    const code = this.$route.query?.code;
    if (typeof code !== "string") {
      return;
    }
    this.sending = true;
    return this.uploadPhotoToS3(code)
      .then(url => {
        const data: UserSignUp = {
          invitationCode: code,
          user: { photoURL: url, ...this.form }
        };
        return this.$store.dispatch("signUp", data);
      })
      .then(() => {
        this.$router.replace({ name: "Home" });
      })
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

  uploadPhotoToS3(code: string): Promise<string | null> {
    const photo = this.photoFile;
    if (!photo) {
      return Promise.resolve(null);
    }
    return ApiService.preSignURL(code).then(data =>
      axios
        .put(data.signedURL, photo, {
          headers: { "Content-Type": photo.type }
        })
        .then(() => data.objectURL)
    );
  }

  onPhotoChange(file: File) {
    (this.$refs.cropper as ModalCropper).show(file);
  }

  onAcceptImage({ blob, type }: { blob: Blob | null; type?: string }) {
    if (blob) {
      this.photoFile = new File([blob], "photo", { type });
      this.photoSrc = window.URL.createObjectURL(blob);
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
