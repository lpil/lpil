const path = require("path");
const merge = require("webpack-merge");
const config = require("./webpack.config");

module.exports = merge(config, {
  entry: path.join(__dirname, "src/index.js"),

  module: {
    loaders: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  "elm-webpack"
      },
    ],
  },

  plugins: [
    new webpack.optimize.OccurenceOrderPlugin(),

    new webpack.optimize.UglifyJsPlugin({
      minimize:   true,
      compressor: { warnings: false }
    }),
  ],
});
