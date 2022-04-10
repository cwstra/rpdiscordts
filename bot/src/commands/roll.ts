import { anyOf } from "@databases/pg-typed";
import { compact, compactMap, rest } from "../helpers/array";
import { midStringCharacters } from "../helpers/string";
import { wrappedTask } from "../interaction-wrapper";
import { makeCommand } from "../make-command";
import { fetchChannelEntry, User } from "../sql-connections";
import { pipe } from "fp-ts/function";
import * as A from "fp-ts/Array";
import * as O from "fp-ts/Option";
import * as R from "fp-ts/Record";
import * as T from "fp-ts/Task";
import * as TO from "fp-ts/TaskOption";
import { groupBy } from "rambda";
import { Falsey } from "../helpers/types";
import { ROLL_PATH, ROLL_PORT, ROLL_URL } from "../env-vars";
import { randomInt } from "fp-ts/lib/Random";
import * as http from "http";

type JSONResult =
  | {
      status: "Failure";
      input: string;
      errorMessage: string;
    }
  | {
      status: "Success";
      input: string;
      history: [string, string][];
      result: string;
    };

module.exports = makeCommand({
  name: "roll",
  description: "Command-invoked dice roller.",
  options: {
    expression: {
      type: "string",
      description: "The roll query to evaluate.",
      required: true,
    },
  },
  execute: wrappedTask((args) => {
    return pipe(
      T.of(args),
      T.chainFirst(
        ({ wrapped }) =>
          () =>
            wrapped.deferReply()
      ),
      T.bind("expression", (a) =>
        pipe(
          a.interaction.guildId,
          TO.fromNullable,
          TO.chain((server_id) =>
            pipe(
              () => fetchChannelEntry(server_id, a.interaction.channelId),
              T.map(O.fromNullable),
              TO.map(({ charseps, charsigns }) => ({
                server_id,
                charseps: compact(charseps ?? [":"]),
                charsigns: compact(charsigns ?? ["$"]),
              }))
            )
          ),
          TO.chainTaskK(({ server_id, charseps, charsigns }) => {
            const [prefix, ...rest] = midStringCharacters(
              a.options.expression,
              charsigns,
              charseps
            );
            return pipe(
              () =>
                User.tables
                  .characters(User.db)
                  .find({
                    server_id,
                    character_name: anyOf(rest.map(([{ name }]) => name)),
                  })
                  .all(),
              T.bindTo("characters"),
              T.bind(
                "attributes",
                ({ characters }) =>
                  () =>
                    User.tables
                      .character_attributes(User.db)
                      .find({
                        character_id: anyOf(
                          characters.map((c) => c.character_id as number)
                        ),
                        name: anyOf(rest.map(([{ prop }]) => prop)),
                      })
                      .all()
              ),
              T.map(({ characters, attributes }) => {
                const splitAttrs = pipe(
                  groupBy((a) => a.character_id.toString(), attributes),
                  R.map((attrs) =>
                    Object.fromEntries(attrs.map((a) => [a.name, a.value]))
                  )
                );
                const lookup = Object.fromEntries(
                  compactMap(
                    characters,
                    (c): [string, Record<string, string>] | Falsey => {
                      const strId = c.character_id.toString();
                      return (
                        strId in splitAttrs && [
                          c.character_name,
                          splitAttrs[strId],
                        ]
                      );
                    }
                  )
                );
                return (
                  prefix +
                  rest
                    .map(
                      ([{ name, prop }, s]) => lookup[name]?.[prop] ?? "" + s
                    )
                    .join("")
                );
              })
            );
          }),
          T.map(O.getOrElse(() => a.options.expression))
        )
      ),
      T.bind(
        "seed",
        T.fromIOK(() => randomInt(0, Number.MAX_SAFE_INTEGER))
      ),
      T.bind(
        "result",
        ({ expression, seed }) =>
          () =>
            fetch(expression, seed)
      ),
      //TODO: Add roll statistics back in
      T.chain(({ wrapped, expression, result }) => async () => {
        wrapped.editReply(formatResult(expression, result));
      })
    );
  }),
});

async function fetch(expression: string, seed: number): Promise<JSONResult> {
  const options = {
    address: ROLL_URL,
    family: 4,
    port: ROLL_PORT,
    path: `${ROLL_PATH}?roll=${encodeURIComponent(expression).replace(
      "'",
      "%27"
    )}&seed=${seed}`,
  };
  return new Promise((resolve, reject) =>
    http
      .request(options, (response) => {
        let output = "";
        response.on("data", (chunk) => (output += chunk));
        response.on("error", (e) => reject(e));
        response.on("end", () => resolve(JSON.parse(output)));
      })
      .end()
  );
}

const block = (s: string) => "```\n" + s + "```";

function formatResult(expression: string, jsonResult: JSONResult): string {
  const inputStr = `${
    expression === jsonResult.input ? "" : `Expression: ${expression}\n`
  }Input: \`${jsonResult.input}\``;
  if (jsonResult.status === "Failure")
    return `Roll Failure:
${inputStr}
Error Message:\`${jsonResult.errorMessage}\``;
  const recombinedHistory =
    jsonResult.history[0][0] === jsonResult.history[0][1]
      ? rest(jsonResult.history)
      : jsonResult.history;
  if (!recombinedHistory.length) return "Nothing to compute.";
  const [firsts, seconds] = A.unzip(recombinedHistory);
  const historyDisplay = A.zipWith(
    firsts,
    seconds,
    (f, s) => `${f}\n-> ${s}`
  ).join("\n");
  return `${inputStr}
${block(historyDisplay)}
Result: ${jsonResult.result}`;
}
