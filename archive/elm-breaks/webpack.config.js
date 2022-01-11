const path              = require("path");
const webpack           = require("webpack");
const merge             = require("webpack-merge");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const commonConfig = {
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

const prodConfig = {
  entry: path.join(__dirname, "src/index.js"),

  module: {
    loaders: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  "elm-webpack"
      }
    ]
  },

  plugins: [
    new webpack.optimize.OccurenceOrderPlugin(),

    new webpack.optimize.UglifyJsPlugin({
      minimize: true,
      compressor: { warnings: false }
    })
  ]
};

const devConfig = {
  entry: [
    "webpack-dev-server/client?http://localhost:8080",
    path.join(__dirname, "src/index.js")
  ],

  devServer: {
    inline:   true,
    progress: true
  },

  module: {
    loaders: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  "elm-hot!elm-webpack?verbose=true&warn=true"
      },
    ],
  },
};

module.exports =
  (process.env.NODE_ENV === "production")
  ? merge(commonConfig, prodConfig)
  : merge(commonConfig, devConfig);
