<template>
  <b-container class="mt-4">
    <b-alert :show="error" variant="danger" dismissible>{{ errorMsg }}</b-alert>
    <b-form @submit.prevent="onSubmit">
      <profile-form v-model="profile" @state="valid = $event" />

      <b-button
        :disabled="sending || !valid"
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
import ApiService from "@/services/api";
import ProfileForm, { ProfileFormData } from "@/components/ProfileForm.vue";

@Component({
  computed: mapGetters(["sessionUser"]),
  components: { ProfileForm }
})
export default class Profile extends Vue {
  profile: ProfileFormData = {
    displayName: "",
    institution: "",
    pronouns: "",
    website: "",
    bio: "",
    photo: {
      file: null,
      previewURL: null
    }
  };
  valid = true;

  error = false;
  errorMsg = "";
  sending = false;

  onSubmit() {
    if (this.sending || !this.valid) {
      return;
    }
    this.sending = true;
    this.submit()
      .then(() => this.$router.push({ name: "Home" }))
      .catch(error => {
        if (error.response?.status == 400) {
          this.setError("The data contain errors");
        } else {
          this.setError("There was an unexpected error");
        }
      })
      .finally(() => (this.sending = false));
  }

  setError(msg: string) {
    this.error = true;
    this.errorMsg = msg;
  }

  async submit() {
    let photoURL = this.$store.getters.sessionUser?.photoURL;
    if (this.profile.photo.file) {
      photoURL = await ApiService.uploadFile(this.profile.photo.file);
    }
    const data: UserData = {
      ...this.profile,
      photoURL
    };
    return this.$store.dispatch("updateUserDataMe", data);
  }

  // We are relying on NavBar calling "syncSessionUser"
  @Watch("sessionUser", { immediate: true })
  sessionUserUpdated(newVal: User | null, oldVal: User | null) {
    // Only update the first time to avoid overriding the data while we are editing it.
    if (newVal && !oldVal) {
      this.profile = {
        photo: {
          file: null,
          previewURL: newVal.photoURL
        },
        ...newVal
      };
    }
  }
}
</script>
