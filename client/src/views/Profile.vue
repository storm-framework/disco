<template>
  <b-container>
    <b-form @submit.prevent="onSubmit">
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
        <b-form-file
          placeholder="Choose a file or drop it here"
          drop-placeholder="Drop file here..."
          v-model="form.photo"
          accept="image/*"
          :disabled="fatalError"
        ></b-form-file>
      </b-form-group>

      <b-button
        :disabled="fatalError || sending"
        variant="primary"
        size="lg"
        type="submit"
        class="mt-4"
      >
        Save
      </b-button>
    </b-form>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue, Watch } from "vue-property-decorator";
import { mapGetters } from "vuex";
import { User, UserData } from "@/models";
import axios from "axios";
import ApiService from "@/services/api";

interface Form {
  firstName: string;
  lastName: string;
  displayName: string;
  institution: string;
  photo: File | null;
}

@Component({ computed: mapGetters(["sessionUser"]) })
export default class Profile extends Vue {
  form: Form = {
    firstName: "",
    lastName: "",
    displayName: "",
    institution: "",
    photo: null
  };
  loading = false;
  fatalError = false;
  errorMsg = "";
  sending = false;

  onSubmit() {
    this.uploadPhotoToS3()
      .then(url => {
        const data: UserData = {
          photoURL: url,
          ...this.form
        };
        return this.$store.dispatch("updateUserDataMe", data);
      })
      .catch(console.log)
      .finally(() => (this.sending = false));
  }
  // We are relying on the NavBar calling "syncSessionUser"
  @Watch("sessionUser", { immediate: true })
  sessionUserUpdated(newVal: User) {
    this.form = { photo: null, ...newVal };
  }

  uploadPhotoToS3() {
    const photo = this.form.photo;
    if (!photo) {
      return Promise.resolve(this.$store.getters.sessionUser?.photoURL);
    }
    return ApiService.preSignURL().then(data =>
      axios
        .put(data.signedURL, this.form.photo, {
          headers: { "Content-Type": photo.type }
        })
        .then(() => data.objectURL)
    );
  }
}
</script>
