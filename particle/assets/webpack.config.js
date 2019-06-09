const path = require("path");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

const isProd = process.env.NODE_ENV == "production";

function cssLoader() {
  if (isProd) {
    return ExtractTextPlugin.extract({
      fallback: "style-loader",
      use: ["css-loader"]
    });
  } else {
    return ["style-loader", "css-loader"];
  }
}

function plugins() {
  if (isProd) {
    return [new ExtractTextPlugin({ filename: "[name].css" })];
  } else {
    return [];
  }
}

module.exports = {
  entry: "./src/Main.bs.js",
  output: {
    path: path.resolve(__dirname, "../priv/static"),
    filename: "main.js"
  },
  module: {
    rules: [{ test: /\.css$/, use: cssLoader() }]
  },
  plugins: plugins()
};
