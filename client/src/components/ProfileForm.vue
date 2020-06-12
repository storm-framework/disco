<template>
  <div>
    <b-form-group
      label="Badge name*"
      label-for="display-name"
      description="People will use your badge name to identify you in the app."
    >
      <b-form-input
        id="display-name"
        type="text"
        :value="value.displayName"
        @input="updateInput('displayName', $event)"
        required
        trim
        placeholder="Badge name"
        :disabled="disabled"
      />
    </b-form-group>

    <b-form-group label="Photo" label-for="photo">
      <photo-input
        :value="value.photo"
        @input="updateInput('photo', $event)"
        :disabled="disabled"
      />
    </b-form-group>

    <b-form-group label="Pronouns" label-for="pronouns">
      <b-form-input
        id="pronouns"
        type="text"
        trim
        :value="value.pronouns"
        @input="updateInput('pronouns', $event)"
        placeholder="they/them"
        :disabled="disabled"
      />
    </b-form-group>

    <b-form-group label="Affiliation" label-for="institution">
      <b-form-input
        id="institution"
        type="text"
        trim
        :value="value.institution"
        @input="updateInput('institution', $event)"
        placeholder="Affiliation"
        :disabled="disabled"
      />
    </b-form-group>

    <b-form-group label="Website" label-for="website">
      <b-form-input
        id="website"
        type="url"
        trim
        :value="value.website"
        @input="updateInput('website', $event)"
        placeholder="Website"
        :disabled="disabled"
      />
    </b-form-group>

    <b-form-group
      label="Bio"
      label-for="website"
      :invalid-feedback="`Your bio cannot exceed ${maxBioSize} characters.`"
      :state="validBio && null"
    >
      <b-form-textarea
        id="bio"
        trim
        type="text"
        :value="value.bio"
        @input="updateInput('bio', $event)"
        :disabled="disabled"
        :state="validBio && null"
      />
    </b-form-group>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import PhotoInput from "@/components/PhotoInput.vue";

type ValidField =
  | "displayName"
  | "photo"
  | "pronouns"
  | "institution"
  | "website"
  | "bio";

export interface ProfileFormData {
  displayName: string;
  photo: {
    file: File | null;
    previewURL: string | null;
  };
  pronouns: string;
  institution: string;
  website: string;
  bio: string;
}

@Component({ components: { PhotoInput } })
export default class ProfileForm extends Vue {
  readonly maxBioSize = 300;

  @Prop({ required: true }) value!: ProfileFormData;
  @Prop({ default: false }) disabled!: boolean;

  get validBio() {
    return this.value.bio.length <= this.maxBioSize;
  }

  @Watch("validBio", { immediate: true })
  emitValid(newValue: boolean, oldValue: boolean) {
    if (newValue != oldValue) {
      this.$emit("state", newValue);
    }
  }

  updateInput(field: ValidField, value: any) {
    const updated = { ...this.value };
    updated[field] = value;
    this.$emit("input", updated);
  }
}
</script>
