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
    // Plugins loaded unconditionally which don't require any special
    // configuration
    plugins: [new LodashModuleReplacementPlugin()],
    devtool: "source-map",
    externals: {
      "jitsi-meet-external-api": "JitsiMeetExternalAPI"
    }
  },
  chainWebpack(config) {
    if (process.env.BUNDLE_ANALYZER) {
      config.plugin("bundle-analyzer").use(BundleAnalyzerPlugin);
    }

    const cert = process.env.WEBPACK_DEV_SERVER_HTTPS_CERT;
    const key = process.env.WEBPACK_DEV_SERVER_HTTPS_KEY;
    const ca = process.env.WEBPACK_DEV_SERVER_HTTPS_CA;

    if (cert && key) {
      config.devServer.https({
        cert,
        key,
        ca
      });
    }

    config
      .plugin("script-ext")
      .use(ScriptExtHtmlPlugin, [
        {
          defaultAttribute: "defer"
        }
      ])
      .after("html");
  }
};
