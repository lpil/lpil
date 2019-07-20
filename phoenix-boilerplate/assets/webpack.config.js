const path = require("path");

module.exports = {
  entry: "./src/Main.bs.js",
  output: {
    path: path.resolve(__dirname, "../priv/static"),
    filename: "main.js"
  }
};
