const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;
const LodashModuleReplacementPlugin = require("lodash-webpack-plugin");

const plugins = [new LodashModuleReplacementPlugin()];

if (process.env.BUNDLE_ANALYZER) {
  plugins.push(new BundleAnalyzerPlugin());
}

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
    plugins
  }
};
