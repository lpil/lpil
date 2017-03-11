const path = require("path");
const merge = require("webpack-merge");
const config = require("./webpack.config");

module.exports = merge(config, {
  entry: [
    "webpack-dev-server/client?http://localhost:8080",
    path.join(__dirname, "src/index.js")
  ],

  devServer: {
    inline: true
  },

  module: {
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-hot-loader!elm-webpack-loader?debug=true&verbose=true&warn=true"
      }
    ]
  }
});
