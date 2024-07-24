import { Ok as OkG, Error as ErrorG } from "./gleam.mjs";
import { ComparisonOutOfBounds, ComparisonFailed } from "./atomic_array.mjs";

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

export function size(array) {
  return array.length;
}

export function set(array, index, value) {
  try {
    Atomics.store(array, index, BigInt(value));
    return new OkG(undefined);
  } catch {
    return new ErrorG(undefined);
  }
}

export function add(array, index, amount) {
  try {
    Atomics.add(array, index, BigInt(amount));
    return new OkG(undefined);
  } catch {
    return new ErrorG(undefined);
  }
}

export function exchange(array, index, value) {
  try {
    const x = Atomics.exchange(array, index, BigInt(value));
    return new OkG(Number(x));
  } catch {
    return new ErrorG(undefined);
  }
}

export function compare_exchange(array, index, expected, value) {
  let previous;
  const e = BigInt(expected);
  const v = BigInt(value);
  try {
    previous = Number(Atomics.compareExchange(array, index, e, v));
  } catch {
    return new ErrorG(new ComparisonOutOfBounds());
  }
  if (previous === expected) {
    return new OkG(undefined);
  }
  return new ErrorG(new ComparisonFailed(previous));
}
