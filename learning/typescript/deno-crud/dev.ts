#!/usr/bin/env -S deno run -A --watch=static/,routes/

import dev from "$fresh/dev.ts";
import { loadDevelopmentConfiguration } from "./lib/configuration.ts";

// Load development secrets into environment variables
const error = loadDevelopmentConfiguration();
if (error) {
  console.error("ERROR: " + error.message);
  Deno.exit(1);
}

await dev(import.meta.url, "./main.ts");
