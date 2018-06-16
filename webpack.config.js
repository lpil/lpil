const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const isProd = process.env.NODE_ENV === "production";
const mode = isProd ? "production" : "development";
const stats = "minimal";

const outputDir = path.join(__dirname, "dist/");
const srcDir = path.join(__dirname, "src/");

const nodeConfig = {
  mode,
  stats,
  target: "node",
  entry: {
    tick_function: "./src/TickFunction.bs.js",
    api_function: "./src/ApiFunction.bs.js"
  },
  output: {
    path: outputDir,
    filename: "[name].js",
    libraryTarget: "commonjs"
  }
};

const webConfig = {
  mode,
  stats,
  entry: {
    website: "./src/Website.bs.js"
  },
  output: {
    path: outputDir,
    filename: "[name].js"
  },
  plugins: [
    new webpack.HotModuleReplacementPlugin(),
    new HtmlWebpackPlugin({ inject: true })
  ],
  devServer: {
    stats,
    inline: true,
    hot: true,
    open: true,
    contentBase: srcDir,
    proxy: {
      "/api": "http://127.0.0.1:50545"
    }
  }
};

module.exports = [nodeConfig, webConfig];
