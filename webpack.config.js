const path = require("path");
const webpack = require("webpack");

const isProd = process.env.NODE_ENV === "production";

const outputDir = path.join(__dirname, "dist/");
const srcDir = path.join(__dirname, "src/");

module.exports = {
  entry: {
    tick: "./src/Main.bs.js"
  },
  target: "node",
  mode: isProd ? "production" : "development",
  output: {
    path: outputDir,
    filename: "[name].js",
    libraryTarget: "commonjs"
  },
  // plugins: [new webpack.HotModuleReplacementPlugin()],
  devServer: {
    stats: "minimal",
    inline: true,
    hot: true,
    open: true,
    contentBase: srcDir,
    proxy: {
      "/api": "http://127.0.0.1:50545"
    }
  }
};
