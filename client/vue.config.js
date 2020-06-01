module.exports = {
  configureWebpack: {
    devtool: "eval-source-map"
  },
  devServer: {
    proxy: {
      "/api": {
        target: "http://127.0.0.1:3000",
        changeOrigin: true
      }
    }
  }
};
