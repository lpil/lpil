var babelPresets = [
  "es2015",
  "react",
];
var babelPlugins = [
  "transform-object-assign",
];

module.exports = {
  entry: { index: "./pages/index.jade" },
  output: {
    path: "./dist/",
    filename: "[name].js",
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
        test: /\.jade$/,
        loader: "file?name=[name].html!jade-html",
      },
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
    ],
  },
};
