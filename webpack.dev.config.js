const webpack = require("webpack");
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
        use: [
          { loader: "elm-hot-loader" },
          { loader: "elm-webpack-loader?debug=true&verbose=true&warn=true" }
        ]
      },
      {
        test: /\.scss$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" },
          { loader: "sass-loader" }
        ]
      }
    ]
  },

  plugins: [new webpack.NamedModulesPlugin()]
});

