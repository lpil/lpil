import {
  new$ as newDict,
  insert as insertDict,
} from "../gleam_stdlib/gleam/dict.mjs";
import { Result$Error, Result$Ok } from "./gleam.mjs";

let os = globalThis?.process?.platform
  || globalThis?.navigator?.userAgentData?.platform?.toLowerCase()
  || "unknown";

if (os === "windows") {
  os = "win32"
} else if (os === "iOS" || os === "macOS") {
  os = "darwin";
} else if (os === "android" || os === "chrome os") {
  os = "linux"
}

export function is_windows() {
  return os == "win32";
}

export function system_name() {
  return os;
}

export function environment_get(key) {
  const value = globalThis?.process?.env[key];

  if (value === undefined) {
    return Result$Error(undefined);
  } else {
    return Result$Ok(value);
  }
}

export function environment_set(key, value) {
  if (globalThis.process?.env) {
    globalThis.process.env[key] = value;
  }
}

export function environment_unset(key) {
  if (globalThis.process?.env) {
    delete process.env[key];
  }
}

export function environment_all() {
  const environmentVariables = globalThis.process?.env ?? {};
  let result = newDict();
  for (const key in environmentVariables) {
    if (Object.hasOwn(environmentVariables, key)) {
      result = insertDict(result, key, environmentVariables[key]);
    }
  }

  return result;
}
