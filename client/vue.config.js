/* eslint-disable @typescript-eslint/no-var-requires */
const { BundleAnalyzerPlugin } = require("webpack-bundle-analyzer");
const LodashModuleReplacementPlugin = require("lodash-webpack-plugin");
const ScriptExtHtmlPlugin = require("script-ext-html-webpack-plugin");

module.exports = {
  productionSourceMap: false,
  devServer: {
    proxy: {
      "/api": {
        target: "http://127.0.0.1:3000",
        changeOrigin: true
      }
    }
  },
  configureWebpack: {
    externals: {
      "jitsi-meet-external-api": "JitsiMeetExternalAPI"
    }
  },
  chainWebpack(config) {
    if (process.env.BUNDLE_ANALYZER) {
      config.plugin("bundle-analyzer").use(BundleAnalyzerPlugin);
    }

    // prettier-ignore
    config
      .plugin("lodash")
        .use(LodashModuleReplacementPlugin)
        .end()
      .plugin("script-ext")
        .use(ScriptExtHtmlPlugin, [
          {
            defaultAttribute: "defer"
          }
        ])
        .after("html");
  }
};
