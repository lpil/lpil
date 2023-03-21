import { parse as parseJsonc } from "std/jsonc/mod.ts";

// Load development secrets from `./secrets.jsonc` into the application
// environment.
//
// In production this file should not exist, instead the secrets should be set
// in environment variables.
//
// Return an error if could not succeed for any reason.
//
export function loadDevelopmentConfiguration(): void | Error {
  let json = "";

  try {
    json = Deno.readTextFileSync("./secrets.jsonc");
  } catch (_error) {
    return new Error("./secrets.jsonc could not be read.");
  }

  let input;
  try {
    input = parseJsonc(json);
  } catch (error) {
    return new Error(
      `Error: ./secrets.jsonc could not be parsed.\n${error.message}`
    );
  }

  if (typeof input !== "object" || input === null)
    return new Error("secrets.jsonc must be an object");

  for (const [key, value] of Object.entries(input)) {
    if (typeof value !== "string")
      return new Error(`secrets.jsonc: ${key} must be a string`);

    if (value.length === 0)
      return new Error(`secrets.jsonc: ${key} must not be empty`);

    Deno.env.set(key, value);
  }
}
