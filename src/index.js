require("./styles/main.scss");

const Auth0Lock = require("auth0-lock").default;
const elmApp = require("./elm-app");
const constants = require("./constants");

const idTokenStorageKey = constants.idTokenStorageKey;
const profileStorageKey = constants.profileStorageKey;

const auth0ClientId = "vrkVChz9uCUe4F8UZx1hcJinOdjDHBGr";
const domain = "lpil.eu.auth0.com";

const lock = new Auth0Lock(auth0ClientId, domain);

function buildFlags(idToken, profile) {
  return {
    endpoint: "https://api.graph.cool/simple/v1/cj05f7nxp1ztj0123t3tfsg1z",
    idToken: idToken,
    userName: profile.name,
    userEmail: profile.email
  };
}

function onAuthenticated(authResult) {
  lock.getProfile(authResult.idToken, function(error, profile) {
    if (error) {
      // Handle error
      console.error(error);
      return;
    }
    const idToken = authResult.idToken;
    localStorage.setItem(idTokenStorageKey, idToken);
    localStorage.setItem(profileStorageKey, JSON.stringify(profile));
    lock.hide();
    const flags = buildFlags(idToken, profile);
    elmApp(flags);
  });
}

const idToken = localStorage.getItem(idTokenStorageKey);
const profileJson = localStorage.getItem(profileStorageKey);

if (idToken && profileJson) {
  const profile = JSON.parse(profileJson);
  const flags = buildFlags(idToken, profile);
  elmApp(flags);
} else {
  lock.show();
  lock.on("authenticated", onAuthenticated);
}
