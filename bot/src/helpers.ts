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

export type FlattenedObjectUnion<Objs extends object> = {
  readonly // The `Objs extends objects` are here to distribute the types
  [K in Objs extends object ? keyof Objs : never]: Objs extends object
    ? K extends keyof Objs
      ? Objs[K]
      : undefined
    : never;
};

export type FlattenedArrayUnion<Arrs extends readonly unknown[]> =
  readonly (Arrs extends (infer E)[] ? E : never)[];

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
