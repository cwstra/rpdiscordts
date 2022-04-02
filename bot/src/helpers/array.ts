import { head, tail, last as lastRamda } from "rambda";
import { Falsey } from "./types";

export function truthy<T>(t: T): t is Exclude<T, Falsey> {
  return !!t;
}

export function compact<T>(arr: T[] | null | undefined): Exclude<T, Falsey>[] {
  return arr?.filter((e): e is Exclude<T, Falsey> => !!e) ?? [];
}
export function compactMap<T, R>(
  arr: T[],
  fn: (t: T) => R
): Exclude<R, Falsey>[] {
  const result: Exclude<R, Falsey>[] = [];
  arr.forEach((e) => {
    const res = fn(e);
    if (truthy(res)) result.push(res);
  });
  return result;
}

export function first(array: readonly []): undefined;
export function first<T>(array: readonly [T, ...unknown[]]): T;
export function first<T>(array: readonly T[]): T | undefined;
export function first(array: readonly unknown[]) {
  return head(array as unknown[]);
}

export function rest(array: readonly []): readonly [];
export function rest<Arr extends unknown[]>(
  array: readonly [unknown, ...Arr[]]
): Arr;
export function rest<T>(array: readonly T[]): T[];
export function rest(array: readonly unknown[]): readonly unknown[] {
  return tail(array as unknown[]);
}

export function last(array: readonly []): undefined;
export function last<T>(array: readonly [...unknown[], T]): T;
export function last<T>(array: readonly T[]): T | undefined;
export function last(array: readonly unknown[]) {
  return lastRamda(array as unknown[]);
}

export function initLast(array: readonly []): [[], undefined];
export function initLast<I, L>(array: readonly [...I[], L]): [I[], L];
export function initLast<T>(array: readonly T[]): [T[], T | undefined];
export function initLast(array: readonly unknown[]) {
  return [array.slice(0, -1), lastRamda(array as unknown[])];
}

export function findApply<T, R>(
  arr: T[],
  fn: (t: T) => R | Falsey
): R | undefined {
  for (let i = 0; i < arr.length; i++) {
    const res = fn(arr[i]);
    if (res) return res;
  }
}
