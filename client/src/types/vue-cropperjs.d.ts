declare module "vue-cropperjs" {
  import Cropper from "cropperjs";
  import { Vue } from "vue-property-decorator";

  class VueCropper extends Vue {
    getCroppedCanvas(
      options?: Cropper.GetCroppedCanvasOptions
    ): HTMLCanvasElement;
    replace(url: string, onlyColorChanged?: boolean): Cropper;
  }

  export default VueCropper;
}
