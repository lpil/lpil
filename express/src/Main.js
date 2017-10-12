exports.makeRequestHandler = function(psApp) {
  const express = require("express");
  const expressApp = express();
  psApp.value0(expressApp)();
  return expressApp;
};
