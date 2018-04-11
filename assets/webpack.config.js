const path = require("path");

module.exports = {
  entry: "./src/main.bs.js",
  output: {
    path: path.resolve(__dirname, "../priv/static"),
    filename: "main.js"
  }
};
