import ExtractText from "extract-text-webpack-plugin";

const config = {
  entry: "./web/static/js/main.js",
  output: {
    path: "./priv/static/assets",
    filename: "[name].js",
  },

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
        loader: "babel-loader",
      },
      {
        test: /\.scss$/,
        loader: ExtractText.extract("style", "css!sass"),
      },
      {
        test: /\.(ttf|eot|svg|woff2?)$/,
        loader : "file-loader?name=fonts/[name]-[hash:6].[ext]",
      },
      {
        test: /\.(png|jpg|gif)$/,
        loader: "url-loader?limit=5000&name=images/[name]-[hash:6].[ext]",
      },
    ],
  },

  plugins: [
    new ExtractText("[name].css", {
      allChunks: true
    })
  ],
};

export default config;
