var babelPresets = [
  "es2015",
  "react",
];
var babelPlugins = [
  "transform-object-assign",
];

module.exports = {
  entry: "./web/static/app.js",
  output: {
    path: "./priv/static",
    filename: "app.js",
  },

  devtool: "inline-source-map",

  module: {
    preLoaders: [
      {
        test: /\.js$/,
        loader: "eslint-loader",
      },
    ],
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "babel-loader",
        query: { presets: babelPresets, plugins: babelPlugins },
      },
      {
        test: /\.scss$/,
        loader: "style!css!sass",
      },
      {
        test: /\.(ttf|eot|svg|woff2?)$/,
        loader : "file-loader?name=fonts/[name].[ext]",
      },
    ],
  },
};
