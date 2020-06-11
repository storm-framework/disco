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
        <photo-input
          :disabled="fatalError"
          v-model="photoURL"
          @fileInput="form.photoFile = $event"
        />
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
import ApiService from "@/services/api";
import PhotoInput from "@/components/PhotoInput.vue";

interface Form {
  firstName: string;
  lastName: string;
  displayName: string;
  institution: string;
  photoFile: File | null;
}

@Component({
  computed: mapGetters(["sessionUser"]),
  components: { PhotoInput }
})
export default class Profile extends Vue {
  form: Form = {
    firstName: "",
    lastName: "",
    displayName: "",
    institution: "",
    photoFile: null
  };
  photoURL: string | null = null;

  loading = false;
  fatalError = false;
  errorMsg = "";
  sending = false;

  onSubmit() {
    this.sending = true;
    this.submit().finally(() => (this.sending = false));
  }

  async submit() {
    let photoURL = this.$store.getters.sessionUser?.photoURL;
    if (this.form.photoFile) {
      photoURL = await ApiService.uploadFile(this.form.photoFile);
    }
    const data: UserData = {
      ...this.form,
      photoURL
    };
    return this.$store.dispatch("updateUserDataMe", data);
  }

  // We are relying on NavBar calling "syncSessionUser"
  @Watch("sessionUser", { immediate: true })
  sessionUserUpdated(newVal: User | null, oldVal: User | null) {
    // Only update the first time to avoid overriding the data while we are editing it.
    if (newVal && !oldVal) {
      this.photoURL = newVal.photoURL;
      this.form = { photoFile: null, ...newVal };
    }
  }
}
</script>
