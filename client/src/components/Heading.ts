import { Vue, Component, Prop } from "vue-property-decorator";
import { CreateElement } from "vue/types";

type Level = 1 | 2 | 3 | 4 | 5 | 6;

@Component
export default class HTag extends Vue {
  @Prop({ required: true }) level!: Level;
  @Prop({ default: 1 }) context!: number;

  private get realLevel(): Level {
    return Math.min(this.level + this.context, 6) as Level;
  }

  render(createElement: CreateElement) {
    return createElement("h" + this.realLevel, this.$slots.default);
  }
}
