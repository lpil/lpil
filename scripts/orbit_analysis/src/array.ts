// Count how many time each string is in the array
export function countEntries(items: string[]): Map<string, number> {
  const counts: Map<string, number> = new Map();
  for (const item of items) {
    const count = counts.get(item) || 0;
    counts.set(item, count + 1);
  }
  return counts;
}
