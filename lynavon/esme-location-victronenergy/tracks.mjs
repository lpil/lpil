const siteId = 420474;
const username = process.env["VICTRON_USERNAME"];
const password = process.env["VICTRON_PASSWORD"];
let accessToken = process.env["VICTRON_ACCESS_TOKEN"];

if (!accessToken) {
  console.log("Gaining bearer token");
  const bearerResponse = await fetch(
    "https://vrmapi.victronenergy.com/v2/auth/login",
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: `{"username":"${username}","password":"${password}"}`,
    },
  );
  if (!bearerResponse.ok) throw new Error(await bearerResponse.text());
  const bearerJson = await bearerResponse.json();
  const bearerToken = bearerJson.token;
  const userId = bearerJson.idUser;

  console.log("Gaining access token");
  const accessResponse = await fetch(
    `https://vrmapi.victronenergy.com/v2/users/${userId}/accesstokens/create`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "x-authorization": `Bearer ${bearerToken}`,
      },
      body: `{"name":"lynavon-esme-tracking-${new Date()}"}`,
    },
  );
  if (!accessResponse.ok) throw new Error(await accessResponse.text());
  const accessJson = await accessResponse.json();
  accessToken = accessJson.token;
}

const start = Date.parse("01-Aug-2024 00:00:00") / 1000;
const end = Date.parse("02-Aug-2024 00:00:00") / 1000;

fetch(
  `https://vrmapi.victronenergy.com/v2/installations/${siteId}/gps-download?start=${start}&end=${end}`,
  {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
      "x-authorization": `Token ${accessToken}`,
    },
  },
)
  .then((res) => res.text())
  .then((json) => console.log(json))
  .catch((err) => console.error("error:" + err));
