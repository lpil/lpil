#!/usr/bin/env -S deno run -A --watch=static/,routes/

import { loadDevelopmentConfiguration } from "./src/configuration.ts";

// Load development secrets into environment variables
const error = loadDevelopmentConfiguration();
if (error) {
  console.error("ERROR: " + error.message);
  Deno.exit(1);
}

const module = await import("./src/main.ts");
module.main();
