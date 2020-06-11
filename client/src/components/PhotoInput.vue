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
      <b-col cols="auto" v-if="src">
        <b-avatar :src="src" />
      </b-col>
      <modal-cropper ref="cropper" v-on:ok="onAcceptImage" />
    </b-form-row>
  </div>
</template>

<script lang="ts">
import "reflect-metadata";
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import ModalCropper from "@/components/ModalCropper.vue";

@Component({ components: { ModalCropper } })
export default class SignIn extends Vue {
  @Prop({ default: false }) disabled!: boolean;
  @Prop() value?: string;

  src: string | null = null;

  onFileChange(file: File) {
    (this.$refs.cropper as ModalCropper).show(file);
  }

  @Watch("value")
  onValueChange(value: string) {
    this.src = value;
  }

  onAcceptImage({ blob, type }: { blob: Blob | null; type?: string }) {
    if (blob) {
      this.$emit("fileInput", new File([blob], "photo", { type }));
      this.src = window.URL.createObjectURL(blob);
      this.$emit("input", this.src);
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
