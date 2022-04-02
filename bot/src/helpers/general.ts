import { FlattenedArrayUnion, FlattenedObjectUnion } from "./types";

export function assert(condition: any, msg?: string): asserts condition {
  if (!condition) {
    throw new Error(msg);
  }
}

export function typeAssert<Base, Extended extends Base>() {
  return;
}

export function flattenedUnion<ObjUnion extends object>(
  unionMember: ObjUnion
): FlattenedObjectUnion<ObjUnion>;
export function flattenedUnion<ObjUnion extends object>(
  unionMember?: ObjUnion
): FlattenedObjectUnion<ObjUnion> | undefined;
export function flattenedUnion<ArrayUnion extends readonly unknown[]>(
  unionMember: ArrayUnion
): FlattenedArrayUnion<ArrayUnion>;
export function flattenedUnion<ArrayUnion extends readonly unknown[]>(
  unionMember?: ArrayUnion
): FlattenedArrayUnion<ArrayUnion> | undefined;
export function flattenedUnion(unionMember: unknown) {
  return unionMember;
}

export function trace<T>(t: T): T {
  console.log(t);
  return t;
}

export function isKeyOf<T>(t: T) {
  return (s: PropertyKey): s is keyof T => s in t;
}
