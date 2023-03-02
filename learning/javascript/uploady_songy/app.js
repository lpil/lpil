import { serve } from "https://deno.land/std@0.114.0/http/server.ts";
import { multiParser } from "https://deno.land/x/multiparser@0.114.0/mod.ts";
import * as uuid from "https://deno.land/std@0.175.0/uuid/mod.ts";
import NodeID3 from "npm:node-id3";

serve(async (req) => {
  if (req.method == "POST") {
    const { files, fields } = await multiParser(req);
    const path = "uploads/" + uuid.v1.generate() + ".mp3";
    await Deno.writeFile(path, files.track.content);
    NodeID3.update({ artist: fields.artist, title: fields.title }, path);
  }
  return new Response(await Deno.readTextFile("./index.html"), {
    headers: { "Content-Type": "text/html; charset=utf-8" },
  });
});
