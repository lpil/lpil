const express = require("express");
const Main = require("../output/Main");

const port = process.env.PORT || 3000;
const app = express();

app.all("*", Main.requestHandler);

app.listen(port, error => {
  if (error) {
    console.error(error.message);
    process.exit(1);
  } else {
    console.log(`listening on localhost:${port}`);
  }
});
