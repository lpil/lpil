var elm = require("./Main.elm");
var app = elm.Main.embed(document.getElementById("main"));

app.ports.play.subscribe(function(sampleId) {
  console.log("play port got ->", sampleId);
});
