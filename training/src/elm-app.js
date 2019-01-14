const constants = require("./constants");
const Elm = require("./Main.elm");

function initElmApp(flags) {
  const app = Elm.Main.embed(document.getElementById("js-app"), flags);

  //
  // Log out of the app
  //
  app.ports.logOut.subscribe(function() {
    localStorage.removeItem(constants.idTokenStorageKey);
    localStorage.removeItem(constants.profileStorageKey);
    location.reload();
  });
}

module.exports = initElmApp;
