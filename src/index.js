// Flag variables are injects via Webpack DefinePlugin
var flags = {
};

var Elm = require("./Main.elm");
console.log(Elm);
Elm.Main.embed(document.getElementById("js-app"), flags);
