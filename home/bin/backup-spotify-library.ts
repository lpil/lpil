#!/usr/bin/env -S deno run --allow-net --allow-write=spotify-library.json

import { serve } from "https://deno.land/std@0.71.0/http/server.ts";

// The port that the local server listens on. Don't change this,
// as Spotify only will redirect to certain predefined URLs.
const port = 43019;

class Spotify {
  auth: string;

  constructor(auth: string) {
    this.auth = auth;
  }

  async call(url: string) {
    let headers = { Authorization: "Bearer " + this.auth };
    let resp = await fetch(url, { headers });
    if (resp.status === 200) {
      return resp.json();
    } else {
      throw await resp.text();
    }
  }

  async list(path: string) {
    let resp = await this.call(path);
    let items = resp.items;
    while (resp.next) {
      resp = await this.call(resp.next);
      items = items.concat(resp.items);
    }
    return items;
  }
}

type Track = {
  uri: string;
  name: string;
  artists: string;
  album: string;
};

class Library {
  playlists: Record<string, Track[]> = {};

  addPlaylist(name: string, tracks: Array<any>) {
    this.playlists[name] = tracks.map(this.toTrack);
  }

  toJson() {
    return JSON.stringify(this.playlists, null, 2);
  }

  private toTrack(track: any): Track {
    return {
      uri: track.track.uri,
      name: track.track.name,
      artists: track.track.artists.map((a: any) => a.name).join(", "),
      album: track.track.album.name
    };
  }
}

console.log(
  "\n\n" +
    "https://accounts.spotify.com/authorize?" +
    "response_type=token&" +
    "client_id=5c098bcc800e45d49e476265bc9b6934&" +
    "scope=playlist-read-collaborative,user-library-read&" +
    `redirect_uri=http://127.0.0.1:${port}/redirect` +
    "\n\n"
);

let server = serve({ port });
let token = "";

for await (const req of server) {
  if (req.url.startsWith("/token")) {
    req.respond({ status: 200, body: "Thank you!" });
    token = /access_token=(?<token>[^&]*)/.exec(req.url)?.groups?.token || "";
    break;
  } else if (req.url.startsWith("/redirect")) {
    let body =
      '<script>location.replace("token?" + location.hash.slice(1));</script>';
    req.respond({ status: 200, body });
  } else {
    req.respond({ status: 404 });
  }
}

let spotify = new Spotify(token);
let me = await spotify.call("https://api.spotify.com/v1/me");

console.log(`Logged in as ${me.id}`);

let playlists = await spotify.list(
  `https://api.spotify.com/v1/users/${me.id}/playlists?limit=50`
);
let library = new Library();

for (let playlist of playlists) {
  console.log(`Fetching playlist ${playlist.name} (${playlist.tracks.total})`);
  let tracks = await spotify.list(playlist.tracks.href + "?limit=100");
  library.addPlaylist(playlist.name, tracks);
}

console.log("Fetching liked tracks");
let liked = await spotify.list("https://api.spotify.com/v1/me/tracks?limit=50");
library.addPlaylist("All liked tracks", liked);

await Deno.writeTextFile("./spotify-library.json", library.toJson());
console.log("Done! Written library to spotify-library.json");

await server.close();
