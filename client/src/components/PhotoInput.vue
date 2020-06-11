<template>
  <div>
    <b-form-row>
      <b-col>
        <b-form-file
          placeholder="Choose a file or drop it here"
          drop-placeholder="Drop file here..."
          accept="image/*"
          @input="onFileChange"
          :disabled="disabled"
        >
        </b-form-file>
      </b-col>
      <b-col cols="auto" v-if="value.previewURL">
        <b-avatar :src="value.previewURL" />
      </b-col>
      <modal-cropper ref="cropper" v-on:ok="onAcceptImage" />
    </b-form-row>
  </div>
</template>

<script lang="ts">
import "reflect-metadata";
import { Component, Vue, Prop } from "vue-property-decorator";

@Component({
  components: { ModalCropper: () => import("@/components/ModalCropper.vue") }
})
export default class SignIn extends Vue {
  @Prop({ default: false }) disabled!: boolean;
  @Prop({ required: true }) value!: {
    file: File | null;
    previewURL: string | null;
  };

  onFileChange(file: File) {
    (this.$refs.cropper as any).show(file);
  }

  onAcceptImage({ blob, type }: { blob: Blob | null; type?: string }) {
    if (blob) {
      const file = new File([blob], "photo", { type });
      const previewURL = window.URL.createObjectURL(blob);
      this.$emit("input", { file, previewURL });
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
