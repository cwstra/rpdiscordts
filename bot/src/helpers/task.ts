import { pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import * as T from "fp-ts/Task";
import * as TE from "fp-ts/TaskEither";

// Bit naughty to allow unsafe tasks, but gosh darn it,
// I want to chain tasks and then have one guard!
export const ensureTask =
  <A>(onRejected: (reason: unknown) => A) =>
  (unsafeTask: T.Task<A>): T.Task<A> =>
    pipe(TE.tryCatch(unsafeTask, onRejected), TE.toUnion);
export const sanitizeTask =
  <E>(onRejected: (reason: unknown) => E) =>
  <A>(unsafeTask: T.Task<A>): TE.TaskEither<E, A> =>
    TE.tryCatch(unsafeTask, onRejected);
export const sanitizeTaskEither =
  <E1>(onRejected: (reason: unknown) => E1) =>
  <E2, A>(unsafeTaskEither: TE.TaskEither<E2, A>): TE.TaskEither<E1 | E2, A> =>
    pipe(TE.tryCatch(unsafeTaskEither, onRejected), T.map(E.flattenW));
