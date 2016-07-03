var HtmlWebpackPlugin = require("html-webpack-plugin");
var ExtractTextPlugin = require("extract-text-webpack-plugin");

module.exports = {
  entry: "./src/main.js",
  output: {
    path: "./dist",
    filename: "main.js",
  },

  devtool: "source-map",

  module: {
    loaders: [
      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract("style-loader", "css-loader")
      },
    ],
  },

  plugins: [
    new HtmlWebpackPlugin({ template: "src/index.html" }),
    new ExtractTextPlugin("main.css"),
  ]
};
