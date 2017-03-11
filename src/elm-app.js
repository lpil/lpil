const Elm = require("./Main.elm");

function initElmApp(flags) {
  Elm.Main.embed(document.getElementById("js-app"), flags);
}

module.exports = initElmApp;
