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

export type FlattenedObjectUnion<Objs extends object> = Simplify<{
  readonly // The `Objs extends objects` are here to distribute the types
  [K in Objs extends object ? keyof Objs : never]: Objs extends object
    ? K extends keyof Objs
      ? Objs[K]
      : undefined
    : never;
}>;

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

export type FlatObject<O> = {
  [K in keyof O]: O[K];
};

export type NonNullProps<O, Keys extends keyof O> = FlatObject<
  O & {
    [K in Keys]-?: Exclude<O[K], null | undefined>;
  }
>;

export type Simplify<T> = T extends object
  ? {
      [K in keyof T]: T[K];
    }
  : T;
