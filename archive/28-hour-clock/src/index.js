require("./styles/main.scss");

const Elm = require("./Main.elm");
Elm.Main.embed(document.getElementById("js-app"));
