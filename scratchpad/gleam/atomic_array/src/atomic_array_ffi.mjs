import { Ok as OkG, Error as ErrorG } from "./gleam.mjs";

export function new_signed(size) {
  const bytes = size * BigInt64Array.BYTES_PER_ELEMENT;
  const buffer = new SharedArrayBuffer(bytes);
  return new BigInt64Array(buffer);
}

export function new_unsigned(size) {
  const bytes = size * BigInt64Array.BYTES_PER_ELEMENT;
  const buffer = new SharedArrayBuffer(bytes);
  return new BigUint64Array(buffer);
}

export function get_or_panic(array, index) {
  return Number(Atomics.load(array, index));
}

export function get(array, index) {
  try {
    return new OkG(Number(Atomics.load(array, index)));
  } catch {
    return new ErrorG(undefined);
  }
}
