module.exports = {
  context: __dirname + "/lib/js/src/",
  entry: "./function.js",
  output: {
    path: __dirname + "/dist",
    filename: "function.js",
    libraryTarget: "commonjs"
  }
};
