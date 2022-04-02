import { Collection } from "discord.js";

export function getOrAdd<K, V>(
  coll: Collection<K, V>,
  key: K,
  makeValue: (key: K) => V
): V {
  const current = coll.get(key);
  // If we only checked for undefined, couldn't support Vs
  // that include undefined.
  if (current !== undefined || coll.has(key)) return current as V;
  const newValue = makeValue(key);
  coll.set(key, newValue);
  return newValue;
}
