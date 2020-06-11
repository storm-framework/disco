const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;
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
    plugins: process.env.BUNDLE_ANALYZER ? [new BundleAnalyzerPlugin()] : []
  }
};
