const newSampler = require("./sampler");

const elm = require("./Main.elm");
const app = elm.Main.embed(document.getElementById("main"));

newSampler().then(sampler => {
  console.log(sampler);
  sampler.play("kick");
});


app.ports.play.subscribe((sampleId) => {
  console.log("play port got ->", sampleId);
});
