require('./main.css');

// Flag variables are injected via Webpack DefinePlugin
var flags = {
};

var Elm = require("./Main.elm");
Elm.Main.embed(document.getElementById("main"), flags);
