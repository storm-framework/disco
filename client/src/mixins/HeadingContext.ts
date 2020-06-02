import { Vue, Component, Prop } from "vue-property-decorator";

@Component
export default class HeadingContext extends Vue {
  @Prop({ default: 1 })
  readonly headingContext!: number;
}
