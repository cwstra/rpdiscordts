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

export type FlatObject<O> = {
  [K in keyof O]: O[K];
};

export type NonNullProps<O, Keys extends keyof O> = FlatObject<
  O & {
    [K in Keys]-?: Exclude<O[K], null | undefined>;
  }
>;
