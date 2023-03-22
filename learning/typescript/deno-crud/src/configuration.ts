import { parse as parseJsonc } from "std/jsonc/mod.ts";
import { catching } from "lib/result.ts";

// Load development secrets from `./secrets.jsonc` into the application
// environment.
//
// In production this file should not exist, instead the secrets should be set
// in environment variables.
//
// Return an error if could not succeed for any reason.
//
export function loadDevelopmentConfiguration(): void | Error {
  const json = catching(() => Deno.readTextFileSync("./secrets.jsonc"));
  if (json instanceof Error)
    return new Error("./secrets.jsonc could not be read.");

  const input = parseJsonc(json);
  if (input instanceof Error)
    return new Error(`secrets.jsonc could not be parsed.\n${input.message}`);

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
