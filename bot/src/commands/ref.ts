import { wrappedCommand, WrappedReplies } from "../interaction-wrapper";
import { makeCommand } from "../make-command";
import { fetchSharedEntry, Server, sql, User } from "../sql-connections";
import { identity, pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import * as T from "fp-ts/Task";
import * as TE from "fp-ts/TaskEither";
import * as TO from "fp-ts/TaskOption";
import * as O from "fp-ts/Option";
import { init, last, splitWhen, sum } from "rambda";
import { checkForGuildAndMember } from "../helpers/commands";
import { isKeyOf } from "../helpers/general";
import { sendPaginatedEmbeds } from "../helpers/paginator";
import { Interaction, Guild } from "discord.js";
import * as fuzzysort from "fuzzysort";

// Temporary(?) measure while we wait for a way to query from views programatically
const entry_list_view = (prefix: string) =>
  sql.__dangerous__rawValue(`${prefix}.entry_list`);
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
    interaction: Interaction & { guild: Guild };
  }
>({
  interaction: { guild, channel },
}: State) =>
  pipe(
    () =>
      fetchSharedEntry({
        target: "channel",
        server_id: guild.id,
        channel_id: channel?.id,
        item: "codex",
      }),
    T.chain(([, , codex]) => TO.fromNullable(codex)),
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


const sanitizeFields = async ({
  interaction,
  embed,
  wrapped,
}: {
  interaction: Parameters<typeof sendPaginatedEmbeds>[0]["interaction"];
  embed: any;
  wrapped: WrappedReplies;
}) => {
  const sanitizedFields = embed.fields
    ? (
        embed.fields as {
          name: string;
          value: string;
          inline?: boolean;
        }[]
      ).flatMap((field) =>
        field.value.length > 1024
          ? splitString(field.value, 1024).map((value, i) => ({
              name: `${field.name} p.${i + 1}`,
              value,
              inline: false,
            }))
          : [field]
      )
    : undefined;
  const splits =
    sanitizedFields?.reduce(
      (acc, f): Exclude<typeof sanitizedFields, undefined>[] =>
        sum(last(acc).map(({ inline }) => (inline ? 1 : 3))) > 9
          ? [...acc, [f]]
          : [...init(acc), [...last(acc), f]],
      [[]] as Exclude<typeof sanitizedFields, undefined>[]
    ) ?? [];
  const embeds =
    splits.length < 4
      ? [embed]
      : splits.map((fields) => ({
          ...embed,
          fields,
        }));
  if (embeds.length > 1)
    await sendPaginatedEmbeds({
      interaction,
      embeds,
    });
  else await wrapped.editReply({ embeds: [embed] });
};

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
          autoComplete: true,
        },
        selected_fields: {
          type: "string",
          description:
            "Space-separated list of fields to include. If not provided or unsupported, all fields are returned.",
          required: false,
        },
      },
      execute: wrappedCommand((args) => {
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
                  TE.fromTask(() =>
                    sanitizeFields({ interaction, embed, wrapped })
                  )
                )
              )
          ),
          TE.getOrElse((s) => async () => {
            await args.wrapped.editReply(s);
          })
        );
      }),
      autoComplete: (interaction, { options }) =>
        pipe(
          { interaction },
          checkForGuildAndMember,
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("tableId", ({ codexPrefix }) =>
            pipe(lookup_view(codexPrefix), TE.of)
          ),
          TE.mapLeft((): string[] => []),
          TE.chainNullableK<string[]>([])((args) =>
            options.entry ? { ...args, entry: options.entry } : undefined
          ),
          TE.chain(({ tableId, entry }) =>
            TE.tryCatch(
              (): Promise<{ id: string }[]> =>
                Server.db.query(sql`
                   select id
                   from ${tableId}
                   where (id % ${entry} or split_part(id, ' (', 1) % ${entry})
                   and (embed ->> 'title' is null or embed ->> 'title' not like '% (Disambiguation)')
                   order by id <-> ${entry}
                   limit 10`),
              (): string[] => []
            )
          ),
          T.map((res) =>
            pipe(
              res,
              E.map((a) => a.map((e) => e.id)),
              E.getOrElse(identity)
            )
          ),
          T.chain(
            (results) => () =>
              interaction.respond(
                results.map((value) => ({ name: value, value }))
              )
          )
        )(),
    }),
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
        count: {
          type: "integer",
          description: "The number of top entries to return; defaults to 5.",
          required: false,
        },
      },
      execute: wrappedCommand((args) =>
        pipe(
          args,
          checkForGuildAndMember,
          TE.chainFirst(({ wrapped }) =>
            TE.fromTask(() => wrapped.deferReply())
          ),
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("tableId", ({ codexPrefix }) =>
            pipe(lookup_view(codexPrefix), TE.of)
          ),
          TE.chain(({ wrapped, tableId, options: { entry, count } }) =>
            pipe(
              TE.tryCatch(
                (): Promise<{ id: string; strong: unknown }[]> =>
                  Server.db.query(sql`
              select id, id % ${entry} as strong
              from ${tableId}
              order by id <-> ${entry}
              limit ${count ?? 5}`),
                () => "Top query failed"
              ),
              TE.chain((results) =>
                TE.fromTask(async () => {
                  const [strong, weak] = splitWhen(
                    (r: typeof results[number]) => !r.strong,
                    results
                  );
                  const sections = [
                    ...(strong.length
                      ? [
                          "Strong results:",
                          strong.map(({ id }) => `\t${id}`).join("\n"),
                        ]
                      : []),
                    ...(weak.length
                      ? [
                          "Weak results:",
                          weak.map(({ id }) => `\t${id}`).join("\n"),
                        ]
                      : []),
                  ];
                  await wrapped.editReply({
                    content: `Top ${count ?? 5} results for ${entry}:
\`\`\`
${sections.join("\n")}
\`\`\``,
                  });
                })
              )
            )
          ),
          TE.getOrElse((s) => async () => {
            await args.wrapped.editReply(s);
          })
        )
      ),
    }),
    makeCommand({
      name: "random_entry",
      description:
        "Look up a random entry from the channel- or server-defined codex.",
      options: {
        category: {
          type: "string",
          description:
            "A codex category to select an entry from. Otherwise, the random entry could come from any category.",
          required: false,
          autoComplete: true,
        },
      },
      execute: wrappedCommand((args) => {
        return pipe(
          args,
          checkForGuildAndMember,
          TE.chainFirst(({ wrapped }) =>
            TE.fromTask(() => wrapped.deferReply())
          ),
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("entryTableId", ({ codexPrefix }) =>
            pipe(entry_list_view(codexPrefix), TE.of)
          ),
          TE.chainTaskK((a) =>
            pipe(
              T.of(a),
              T.bind("category", ({ entryTableId, options: { category } }) =>
                pipe(
                  category === null
                    ? T.of(O.none)
                    : pipe(
                        () =>
                          Server.db.query(sql`
                  select distinct category
                  from ${entryTableId}`),
                        T.map((ls) =>
                          ls.some((l) => l.category === category)
                            ? O.some(category)
                            : O.none
                        )
                      )
                )
              )
            )
          ),
          TE.bindW("lookupTableId", ({ codexPrefix }) =>
            pipe(lookup_view(codexPrefix), TE.of)
          ),
          TE.chain(
            ({ category, interaction, entryTableId, lookupTableId, wrapped }) =>
              pipe(
                category,
                O.match(
                  () => () =>
                    Server.db.query(sql`
              select embed
              from (select *
                    from ${entryTableId}
                    tablesample bernoulli(1)
                    order by random() limit 1) e
              join ${lookupTableId} l
                on e.id = l.id`),
                  (cat) => () =>
                    Server.db.query(sql`
              select embed
              from (select *
                    from ${entryTableId}
                    where category=${cat}
                    order by random() limit 1) e
              join ${lookupTableId} l
                on e.id = l.id`)
                ),
                (id) => id,
                T.map(
                  E.fromPredicate(
                    (data) => !!data.length,
                    () => `Sorry, I couldn't any entries.`
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
                        ).flatMap((o) => Object.values(o)),
                      }
                    : embed
                ),
                TE.chain((embed) =>
                  TE.fromTask(async () =>
                    sanitizeFields({ interaction, embed, wrapped })
                  )
                )
              )
          ),
          TE.getOrElse((s) => async () => {
            await args.wrapped.editReply(s);
          })
        );
      }),
      autoComplete: (interaction, { options }) =>
        pipe(
          { interaction },
          checkForGuildAndMember,
          TE.bind("codexPrefix", getCodexPrefix),
          TE.bindW("tableId", ({ codexPrefix }) =>
            pipe(entry_list_view(codexPrefix), TE.of)
          ),
          TE.mapLeft((): string[] => []),
          TE.chainNullableK<string[]>([])((args) =>
            options.category ? { ...args, entry: options.category } : undefined
          ),
          TE.chain(({ tableId, entry }) =>
            TE.tryCatch(
              (): Promise<{ category: string }[]> =>
                Server.db.query(sql`
                   select category, score
		   from (select distinct category, category <-> ${entry} as score
			 from ${tableId}
			 where (category % ${entry})
		        ) res
	           order by score
		   limit 10`),
              (): string[] => []
            )
          ),
          T.map((res) =>
            pipe(
              res,
              E.map((a) => a.map((e) => e.category)),
              E.getOrElse(identity)
            )
          ),
          T.chain(
            (results) => () =>
              interaction.respond(
                results.map((value) => ({ name: value, value }))
              )
          )
        )(),
    }),
  ],
};
