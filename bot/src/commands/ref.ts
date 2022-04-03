import { wrappedTask } from "../interaction-wrapper";
import { makeCommand } from "../make-command";
import { fetchSharedEntry, Server, sql, User } from "../sql-connections";
import { pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import * as T from "fp-ts/Task";
import * as TE from "fp-ts/TaskEither";
import * as TO from "fp-ts/TaskOption";
import { splitEvery } from "rambda";
import { checkForGuildAndMember } from "../helpers/commands";
import { isKeyOf } from "../helpers/general";
import { sendPaginatedEmbeds } from "../helpers/paginator";
import { CommandInteraction, Guild } from "discord.js";
import * as fuzzysort from "fuzzysort";

// Temporary(?) measure while we wait for a way to query from views programatically
const lookup_view = (prefix: string) =>
  sql.__dangerous__rawValue(`${prefix}.lookup`);

const splitString = (str: string, len: number) => {
  //if (str.length <= len) return [str];
  const out: string[] = [];
  let ind = 0;
  while (ind < str.length) {
    out.push(str.slice(ind, ind + len));
    ind += len;
  }
  return out;
};

const getCodexPrefix = <
  State extends {
    interaction: CommandInteraction & { guild: Guild };
  }
>({
  interaction: { guild, channel },
}: State) =>
  pipe(
    () => fetchSharedEntry("channel", guild.id, channel?.id),
    T.chain(([, e]) => TO.fromNullable(e?.codex)),
    TE.fromTaskOption(
      () => "Please select a codex for this server or channel."
    ),
    TE.chain((id) =>
      pipe(
        () => User.tables.codex_list(User.db).findOne({ id }),
        T.chain((e) => TO.fromNullable(e?.prefix)),
        TE.fromTaskOption(
          () =>
            "Hm. That's odd. You're set up to use a codex that doesn't exist"
        )
      )
    ),
    TE.chain(
      TE.fromPredicate(
        isKeyOf(Server.codexTables),
        () =>
          "Hm. That's odd. I found your codex, but I couldn't find its data."
      )
    )
  );

module.exports = {
  commands: [
    makeCommand({
      name: "ref",
      description: "Look up data from the channel- or server-defined codex.",
      options: {
        entry: {
          type: "string",
          description: "The entry title text to search for.",
          required: true,
        },
        selected_fields: {
          type: "string",
          description:
            "Space-separated list of fields to include. If not provided or unsupported, all fields are returned.",
          required: false,
        },
      },
      execute: wrappedTask((args) => {
        return pipe(
          args,
          checkForGuildAndMember,
          TE.chainFirst(({ wrapped }) =>
            TE.fromTask(() => wrapped.deferReply())
          ),
          TE.bind("fields", ({ options: { selected_fields } }) =>
            TE.of(selected_fields?.split(/\s+/))
          ),
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("tableId", ({ codexPrefix }) =>
            pipe(lookup_view(codexPrefix), TE.of)
          ),
          TE.chain(
            ({ interaction, wrapped, tableId, options: { entry }, fields }) =>
              pipe(
                () =>
                  Server.db.query(sql`
              select embed
              from ${tableId}
              where id % ${entry}
              order by id <-> ${entry}
              limit 1`),
                T.map(
                  E.fromPredicate(
                    (data) => !!data.length,
                    () => `Sorry, I couldn't find a match for ${entry}.`
                  )
                ),
                TE.map(([{ embed }]) =>
                  "extra_fields" in embed
                    ? {
                        ...embed.init,
                        fields: (
                          embed.extra_fields as Record<
                            string,
                            { name: string; value: string }
                          >[]
                        ).flatMap(
                          fields
                            ? (o) =>
                                fields.some(
                                  (f) =>
                                    fuzzysort.go(
                                      f,
                                      Object.values(o).map(({ name }) => name)
                                    ).length
                                )
                                  ? Object.values(o)
                                  : []
                            : (o) => Object.values(o)
                        ),
                      }
                    : embed
                ),
                TE.chain((embed) =>
                  TE.fromTask(async () => {
                    const sanitizedFields = embed.fields
                      ? (
                          embed.fields as { name: string; value: string }[]
                        ).flatMap((field) =>
                          field.value.length > 1024
                            ? splitString(field.value, 1024).map(
                                (value, i) => ({
                                  name: `${field.name} p.${i + 1}`,
                                  value,
                                })
                              )
                            : [field]
                        )
                      : undefined;
                    const embeds =
                      !sanitizedFields || sanitizedFields.length < 4
                        ? [embed]
                        : splitEvery(3, sanitizedFields).map((fields) => ({
                            ...embed,
                            fields,
                          }));
                    if (embeds.length > 1)
                      await sendPaginatedEmbeds({
                        interaction,
                        embeds,
                      });
                    else await wrapped.editReply({ embeds: [embed] });
                  })
                )
              )
          ),
          TE.getOrElse((s) => async () => {
            await args.wrapped.editReply(s);
          })
        );
      }),
    }),
    /*
    makeCommand({
      name: "top",
      description:
        "Look up top results from the channel- or server-defined codex.",
      options: {
        entry: {
          type: "string",
          description: "The entry title text to search for.",
          required: true,
        },
      },
      execute: wrappedTask((args) => {
        return pipe(
          args,
          checkForGuildAndMember,
          TE.chainFirst(({ wrapped }) =>
            TE.fromTask(() => wrapped.deferReply())
          ),
          TE.bind("fields", ({ options: { selected_fields } }) =>
            TE.of(selected_fields?.split(/\s+/))
          ),
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("tableId", ({ codexPrefix }) =>
            pipe(lookup_view(codexPrefix), TE.of)
          ),
          TE.chain(({ interaction, wrapped, tableId, options: { entry } }) =>
            pipe(
              () =>
                Server.db.query(sql`
              select embed
              from ${tableId}
              where id % ${entry}
              order by id <-> ${entry}
              limit 1`),
              T.map(
                E.fromPredicate(
                  (data) => !!data.length,
                  () => `Sorry, I couldn't find a match for ${entry}.`
                )
              ),
              TE.map(([{ embed }]) =>
                "extra_fields" in embed
                  ? {
                      ...embed.init,
                      fields: trace(
                        embed.extra_fields.flatMap((o: object) =>
                          Object.values(o)
                        )
                      ),
                    }
                  : embed
              ),
              TE.chain((embed) =>
                TE.fromTask(async () => {
                  const sanitizedFields = embed.fields
                    ? (
                        embed.fields as { name: string; value: string }[]
                      ).flatMap((field) =>
                        field.value.length > 1024
                          ? splitString(field.value, 1024).map((value, i) => ({
                              name: `${field.name} p.${i + 1}`,
                              value,
                            }))
                          : [field]
                      )
                    : undefined;
                  const embeds =
                    !sanitizedFields || sanitizedFields.length < 4
                      ? [embed]
                      : splitEvery(3, sanitizedFields).map((fields) => ({
                          ...embed,
                          fields,
                        }));
                  if (embeds.length > 1)
                    await sendPaginatedEmbeds({
                      interaction,
                      embeds,
                    });
                  else await wrapped.editReply({ embeds: [embed] });
                })
              )
            )
          ),
          TE.getOrElse((s) => async () => {
            await args.wrapped.editReply(s);
          })
        );
      }),
    }),*/
  ],
};
