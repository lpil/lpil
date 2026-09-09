export function is_windows() {
  return (
    globalThis?.process?.platform === "win32" ||
    globalThis?.Deno?.build?.os === "windows"
  );
}

export function environment_get(key) {
  let value;

  if (globalThis.Deno) {
    value = Deno.env.get(key);
  } else if (globalThis.process) {
    value = process.env[key];
  }

  if (value === undefined) {
    return Result$Error(undefined);
  } else {
    return Result$Ok(value);
  }
}

export function environment_set(key, value) {
  if (globalThis.Deno) {
    Deno.env.set(key, value);
  } else if (globalThis.process) {
    process.env[key] = value;
  }
}

export function environment_unset(key) {
  if (globalThis.Deno) {
    Deno.env.delete(key);
  } else if (globalThis.process) {
    delete process.env[key];
  }
}

export function environment_all() {
  let environmentVariables = {};

  if (globalThis.Deno) {
    environmentVariables = Deno.env.toObject();
  } else if (globalThis.process) {
    environmentVariables = process.env;
  }

  let result = newDict();
  for (let key in environmentVariables) {
    if (Object.hasOwn(environmentVariables, key)) {
      result = insert(result, key, environmentVariables[key]);
    }
  }

  return result;
}
