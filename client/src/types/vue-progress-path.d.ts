declare module "vue-progress-path" {
  import { PluginObject } from "vue";
  export interface PgOptions {
    installComponent: boolean;
    componentsPrefix: string;
    defaultShape: string;
  }
  const plugin: PluginObject<PgOptions>;

  export default plugin;
}
