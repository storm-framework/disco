<template>
  <b-modal
    size="lg"
    header-bg-variant="primary"
    header-text-variant="light"
    body-bg-variant="light"
    body-text-variant="dark"
    cancel-variant="outline-secondary"
    ok-title="Save"
    class="mt-4"
    :id="id"
    title="Profile"
    @ok.prevent="onSave"
    no-close-on-esc
    :no-close-on-backdrop="sending"
    :ok-disabled="sending || !valid"
    :cancel-disabled="sending"
  >
    <b-alert :show="error" variant="danger" dismissible>{{ errorMsg }}</b-alert>
    <b-form @submit.prevent="onSubmit">
      <profile-form v-model="profile" @state="valid = $event" />
    </b-form>
  </b-modal>
</template>

<script lang="ts">
import { Component, Vue, Watch, Prop } from "vue-property-decorator";
import { mapGetters } from "vuex";
import { User, UserData } from "@/models";
import ApiService from "@/services/api";
import ProfileForm, { ProfileFormData } from "@/components/ProfileForm.vue";
import { BvModalEvent, BModal } from "bootstrap-vue";

@Component({
  computed: mapGetters(["sessionUser"]),
  components: { ProfileForm }
})
export default class Profile extends Vue {
  @Prop({ default: null }) readonly id!: string | null;

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

  onSave(ev: BvModalEvent) {
    if (this.sending || !this.valid) {
      return;
    }
    this.sending = true;
    this.submit()
      .then(() => {
        (ev.vueTarget as BModal).hide("profile-saved");
      })
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
