var webpack = require("karma-webpack");

module.exports = function(config) {
  config.set({

    basePath:   ".",
    reporters:  ["progress"],
    frameworks: ["mocha", "chai"],
    colors:     true,
    files: [
      "src/**/*test.js",
    ],

    browsers: ["PhantomJS"],
    preprocessors: {
      "src/**/*.js":  ["webpack"],
    },

    webpack: require("./webpack.config.js"),
    webpackMiddleware: { noInfo: true },
  });
};
