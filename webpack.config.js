const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const merge             = require("webpack-merge");

module.exports = {
  output: {
    path:     path.resolve(__dirname, "dist/"),
    filename: "[hash].js",
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.html",
      inject:   "body",
      filename: "index.html"
    }),
  ],
}
