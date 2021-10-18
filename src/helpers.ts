export function assert(condition: any, msg?: string): asserts condition {
  if (!condition) {
    throw new Error(msg);
  }
}

export type Falsey =
  | false
  | 0
  | -0
  | 0.0
  | 0x0
  | 0n
  | -0n
  | ""
  | null
  | undefined;

export function compact<T>(arr: T[]): Exclude<T, Falsey>[] {
  return arr.filter((e): e is Exclude<T, Falsey> => !!e);
}
