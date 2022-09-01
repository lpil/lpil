// Return a new object containing only pairs for which the function returns true
export function filter<K, V>(
  object: Map<K, V>,
  predicate: (key: K, value: V) => boolean
): Map<K, V> {
  const result = new Map<K, V>();
  for (const [key, value] of object) {
    if (predicate(key, value)) result.set(key, value);
  }
  return result;
}
