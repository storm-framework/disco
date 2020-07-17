<template>
  <b-modal
    no-fade
    size="lg"
    ref="modal"
    v-on:ok="onOk"
    cancel-variant="outline-secondary"
    title="Crop your photo"
  >
    <b-overlay
      :show="loading"
      spinner-variant="primary"
      spinner-type="grow"
      spinner-small
      rounded="sm"
    >
      <div class="img-cropper">
        <vue-cropper
          ref="cropper"
          :aspect-ratio="1"
          :src="src"
          :minCropBoxWidth="50"
        />
      </div>
    </b-overlay>
  </b-modal>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import "cropperjs/dist/cropper.css";
import VueCropper from "vue-cropperjs";
import { BModal } from "bootstrap-vue";

@Component({ components: { VueCropper } })
export default class ModalCropper extends Vue {
  loading = false;
  type?: string;
  src: string | null = null;

  show(file: File) {
    // Set it to null to avoid showing previous image
    this.src = null;
    this.type = file.type;

    (this.$refs.modal as BModal).show();
    const reader = new FileReader();
    this.loading = true;
    reader.onload = () => {
      this.loading = false;
      this.src = reader.result as string;
      this.cropper()?.replace(this.src);
    };
    reader.readAsDataURL(file);
  }

  onOk() {
    this.loading = true;
    this.cropper()
      ?.getCroppedCanvas()
      .toBlob(blob => {
        this.loading = false;
        this.$emit("ok", { blob, type: this.type });
      }, this.type);
  }

  cropper(): VueCropper | null {
    return this.$refs["cropper"] as VueCropper;
  }
}
</script>

<style lang="scss">
.img-cropper {
  min-height: 200px;

  img {
    max-height: 65vh;
  }
}
</style>
