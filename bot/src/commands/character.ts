import { anyOf } from "@databases/pg-typed";
import {
  checkForGuildAndMember,
  InteractionFromGuild,
} from "../helpers/commands";
import { stringListing } from "../helpers/string";
import { wrappedCommand } from "../interaction-wrapper";
import { makeCommand } from "../make-command";
import { sql, User } from "../sql-connections";
import { identity, pipe } from "fp-ts/function";
import * as E from "fp-ts/Either";
import * as T from "fp-ts/Task";
import * as TE from "fp-ts/TaskEither";
import Characters from "../generated_schema/user_data/characters";
import { partitionMap } from "fp-ts/lib/Array";
import { sanitizeTask } from "../helpers/task";
import { GuildMember, Permissions } from "discord.js";
import { flattenedUnion } from "../helpers/types";
import * as tsp from "typescript-parsec"

const tripleBackTick = "```";

// NOTE: Make sure that charseps + charsigns are not present in a character name.

module.exports = makeCommand({
  name: "character",
  description: "Character management commands.",
  subcommands: {
    new: {
      description: "Add a new character.",
      options: {
        name: {
          type: "string",
          description: "The name of the new character.",
          required: true,
        },
        attributes: {
          type: "string",
          description:
            "Zero or more `Name=Value` pairs. If Name or Value contains spaces, quotation marks are necessary.",
          required: false,
        },
      },
    },
    view: {
      description: "View an existing character.",
      options: {
        name: {
          type: "string",
          description: "The name of the character.",
          required: true,
          autoComplete: true,
        },
        attributes: {
          type: "string",
          description:
            "A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary.",
          required: false,
        },
      },
    },
    rename: {
      description: "Change an existing character's name.",
      options: {
        old_name: {
          type: "string",
          description: "The name of the character.",
          required: true,
          autoComplete: true,
        },
        new_name: {
          type: "string",
          description:
            "The new name for the character. The command will fail if this name is already in use in this server.",
          required: true,
        },
      },
    },
    edit: {
      description:
        "Edit an existing character. Name=Attribute pairs are added/edited; lone attributes are deleted.",
      options: {
        name: {
          type: "string",
          description: "The name of the character.",
          required: true,
          autoComplete: true,
        },
        attributes: {
          type: "string",
          description:
          "A space separated list of `Name=Attribute` pairs or lone `Name`s.",
          required: false,
        },
      },
    },
    delete: {
      description: "Delete a character.",
      options: {
        name: {
          type: "string",
          description: "The name of the character.",
          required: true,
          autoComplete: true,
        },
      },
    },
  },
  execute: wrappedCommand((args) => {
    return pipe(
      args,
      checkForGuildAndMember,
      TE.chain(({ interaction, options }) => {
        switch (options.subCommand) {
          case "new": {
            const { name } = options.options;
            const attributes = parseSetAttrArgs(options.options.attributes);
            return pipe(
              () =>
                User.tables.characters(User.db).findOne({
                  server_id: interaction.guild.id,
                  character_name: name,
                }),
              TE.fromTask,
              TE.filterOrElse(
                (currentCharacter): currentCharacter is null =>
                  !currentCharacter,
                () => "This server already has a character with that name!"
              ),
              TE.chainTaskK(
                () => () =>
                  User.tables.characters(User.db).insert({
                    server_id: interaction.guild.id,
                    member_id: interaction.member.id,
                    character_name: name,
                  })
              ),
              TE.chain(([newChar]) => {
                const entries = Object.values(attributes.toUpsert ?? {}).filter(
                  (e): e is [string, string] => !!e[1]
                );
                if (!entries.length)
                  return TE.left(`Character \`${name}\` successfully created!`);
                return pipe(
                  () =>
                    User.tables.character_attributes(User.db).insert(
                      ...entries.map(([name, value]) => ({
                        character_id: newChar.character_id as number,
                        name,
                        value,
                      }))
                    ),
                  T.map(
                    () =>
                      `Character \`${name}\` successfully created, with attribute${
                        entries.length > 1 ? "s" : ""
                      } ${stringListing(entries.map(([k]) => k))}!`
                  ),
                  TE.leftTask
                );
              })
            );
          }
          case "view": {
            const { name } = options.options;
            return pipe(
              getExistingCharacter({ name, interaction }),
              TE.bindTo("currentCharacter"),
              TE.bind("displayName", ({ currentCharacter }) =>
                TE.fromTask(
                  async () =>
                    (
                      await interaction.guild.members.fetch(
                        currentCharacter.member_id
                      )
                    )?.displayName
                )
              ),
              TE.map((args) => ({
                ...args,
                attributeNames: parseGetAttrArgs(options.options.attributes),
              })),
              TE.chain(({ currentCharacter, attributeNames }) =>
                TE.leftTask(
                  attributeNames.length
                    ? pipe(
                        () =>
                          User.tables
                            .character_attributes(User.db)
                            .find({
                              character_id:
                                currentCharacter.character_id as number,
                              name: anyOf(attributeNames),
                            })
                            .select("name", "value")
                            .all(),
                        T.map((attributes) =>
                          attributes.length
                            ? `${name}'s requested attributes are:\n${tripleBackTick}\n${
                                attributes
                                  .map((a) => `${a.name}: ${a.value}`)
                                  .join("\n") + tripleBackTick
                              }`
                            : `${name} does not have any of the requested attributes.`
                        )
                      )
                    : pipe(
                        () =>
                          User.tables
                            .character_attributes(User.db)
                            .find({
                              character_id:
                                currentCharacter.character_id as number,
                            })
                            .select("name")
                            .all(),
                        T.bindTo("attributes"),
                        T.bind(
                          "creator",
                          () => () =>
                            interaction.guild.members.fetch(
                              currentCharacter.member_id
                            )
                        ),
                        T.map(
                          ({ attributes, creator }) =>
                            `The character \`${name}\`, created by ${
                              creator.displayName
                            }, has ${
                              attributes.length
                                ? `the following attributes:\n${tripleBackTick}\n` +
                                  attributes.map((a) => a.name).join("\n") +
                                  tripleBackTick
                                : "no attributes"
                            }`
                        )
                      )
                )
              )
            );
          }
          case "rename": {
            const { old_name, new_name } = options.options;
            return pipe(
              getExistingCharacter({ name: old_name, interaction }),
              adminGuard(interaction),
              TE.chain((currentCharacter) =>
                pipe(
                  getExistingCharacter({ name: new_name, interaction }),
                  TE.matchW(
                    () => E.right(currentCharacter),
                    () =>
                      E.left(
                        `Sorry, there's already a character named ${new_name} in this server.`
                      )
                  )
                )
              ),
              TE.chain(({ character_id }) =>
                pipe(
                  () =>
                    User.tables
                      .characters(User.db)
                      .update({ character_id }, { character_name: new_name }),
                  sanitizeTask(
                    () =>
                      `Hm. Something went wrong while renaming \`${old_name}\` to \`${new_name}\`. Maybe try again?`
                  ),
                  TE.chain(() =>
                    TE.left(
                      `Character \`${old_name}\` renamed to \`${new_name}\`!`
                    )
                  )
                )
              )
            );
          }
          case "edit": {
            const { name } = options.options;
            return pipe(
              getExistingCharacter({ name, interaction }),
              adminGuard(interaction),
              TE.chain((currentCharacter) => {
                const { toUpsert, toDelete } = parseSetAttrArgs(
                  options.options.attributes
                );
                return pipe(
                  () =>
                    User.db.tx(async (db) => {
                      const character_id =
                        currentCharacter.character_id as number;
                      if (toUpsert)
                        await User.tables
                          .character_attributes(db)
                          .insertOrUpdate(
                            ["character_id", "name"],
                            ...toUpsert.map(([name, value]) => ({
                              character_id,
                              name,
                              value,
                            }))
                          );
                      if (toDelete)
                        await User.tables
                          .character_attributes(db)
                          .delete({ character_id, name: anyOf(toDelete) });
                    }),
                  sanitizeTask(
                    () =>
                      `Hm. Something went wrong while updating \`${name}\`'s attributes. Maybe try again?`
                  ),
                  TE.chain(() => TE.left(`Updated \`${name}\`'s attributes!`))
                );
              })
            );
          }
          case "delete": {
            const { name } = options.options;
            return pipe(
              getExistingCharacter({ name, interaction }),
              adminGuard(interaction),
              TE.chain(({ character_id }) =>
                pipe(
                  () =>
                    User.db.tx(async (db) => {
                      await User.tables
                        .character_attributes(db)
                        .delete({ character_id: character_id as number });
                      await User.tables.characters(db).delete({ character_id });
                    }),
                  sanitizeTask(
                    () =>
                      `Hm. Something went wrong while deleting \`${name}\`. See if that character was deleted, and if not, maybe try again?`
                  ),
                  TE.chain(() => TE.left(`Character \`${name}\` deleted!`))
                )
              )
            );
          }
        }
      }),
      TE.getOrElse((message) => () => args.wrapped.reply(message))
    );
  }),
  autoComplete: (args, opts) => {
    const flatOptions = flattenedUnion(opts.options);
    const name = flatOptions[opts.focusedOption] || undefined;
    return pipe(
      TE.tryCatch(
        (): Promise<{ character_name: string }[]> =>
          User.db.query(
            name
              ? opts.subCommand === "view"
                ? sql`
              select character_name
              from characters
              where server_id = ${args.guildId}
              and to_tsquery(${name} || ':*') @@ to_tsvector(character_name)
              order by ts_rank(to_tsvector(character_name), to_tsquery(${name} || ':*')) desc
              limit 10;`
                : sql`
              select character_name
              from characters
              where server_id = ${args.guildId}
              and member_id = ${args.user.id}
              and to_tsquery(${name} || ':*') @@ to_tsvector(character_name)
              order by ts_rank(to_tsvector(character_name), to_tsquery(${name} || ':*')) desc
              limit 10;`
              : opts.subCommand === "view"
              ? sql`
              select character_name
              from characters
              where server_id = ${args.guildId}
              order by character_name
              limit 10;`
              : sql`
              select character_name
              from characters
              where server_id = ${args.guildId}
              and member_id = ${args.user.id}
              order by character_name
              limit 10;`
          ),
        (): { character_name: string }[] => []
      ),
      T.map((res) => pipe(res, E.getOrElse(identity))),
      T.chain(
        (characters) => () =>
          args.respond(
            characters.map(({ character_name: name }) => ({
              name,
              value: name,
            }))
          )
      )
    )();
  },
});
function getExistingCharacter({
  name,
  interaction,
}: {
  name: string;
  interaction: InteractionFromGuild;
}) {
  return pipe(
    () =>
      User.tables.characters(User.db).findOne({
        server_id: interaction.guild.id,
        character_name: name,
      }),
    T.map(
      E.fromNullable("This server doesn't have a character with that name!")
    )
  );
}
function adminGuard(interaction: InteractionFromGuild) {
  return TE.chain((currentCharacter: Characters) =>
    currentCharacter.member_id === interaction.member.id
      ? TE.of(currentCharacter)
      : pipe(
          () => adminRoleCheck(interaction.member, interaction.guild.ownerId),
          T.map((a) => a?.isAdminRole),
          T.chain((isAdminRole) => async () => {
            if (isAdminRole) return E.of(currentCharacter);
            const { displayName } = await interaction.guild.members.fetch(
              currentCharacter.member_id
            );
            return E.left(
              `That character was made by someone else, namely ${displayName}. Only the original creator, an admin, or a server member with a bot permissions role can edit it.`
            );
          })
        )
  );
}

const [parseGetAttrArgs, parseSetAttrArgs] = (() => {
  const lexer = tsp.buildLexer([
    [true, /^=/g, "Equals" as const],
    [true, /^[a-zA-Z0-9_-]+/g, "Unquoted" as const],
    [true, /^"((?:[^"\\]|\\.)*)"/g, "Quoted" as const],
    [false, /^\s+/g, "Space" as const],
  ]);
  type TokenKind = typeof lexer extends tsp.Lexer<infer K> ? K : never;

  const VALUE = tsp.rule<TokenKind, string>();
  const UPSERT = tsp.rule<TokenKind, [string, string]>();
  const GET_ARGS = tsp.rule<TokenKind, string[]>();
  const SET_ARGS = tsp.rule<TokenKind, ([string, string] | string)[]>();

  VALUE.setPattern(
    tsp.alt(
      tsp.apply(tsp.tok("Unquoted" as const), v => v.text),
      tsp.apply(tsp.tok("Quoted" as const), v => v.text.slice(1, -1)),
    )
  );

  UPSERT.setPattern(
    tsp.apply(tsp.seq(VALUE, tsp.str("="), VALUE), (([n, _, v]) => [n, v]))
  );

  GET_ARGS.setPattern(tsp.rep_sc(VALUE));

  SET_ARGS.setPattern(tsp.rep_sc(tsp.alt(UPSERT, VALUE)));

  return [
    (arg: string | null): string[] => {
      if (!arg) return []
      return tsp.expectSingleResult(tsp.expectEOF(GET_ARGS.parse(lexer.parse(arg))))
    },
    (arg: string | null) => {
      if (!arg) return {toUpsert: [], toDelete: []}
      const {left: toUpsert, right: toDelete} =
        pipe(
          tsp.expectSingleResult(tsp.expectEOF(SET_ARGS.parse(lexer.parse(arg)))),
          partitionMap(r => typeof r === 'string' ? E.right(r) : E.left(r))
        )
      return {toUpsert, toDelete}
    },
  ]
})()

export async function adminRoleCheck(
  member: GuildMember,
  ownerId: string,
  isForRoleChange?: boolean
): Promise<{ isAdminRole: boolean; modRoles: string[] }> {
  const isAdmin =
    member.id === ownerId &&
    member.permissions.has(Permissions.FLAGS.ADMINISTRATOR);
  if (isAdmin && !isForRoleChange) return { isAdminRole: true, modRoles: [] };
  const modRoles = (
    await User.tables
      .server_mod_roles(User.db)
      .find({ server_id: member.guild.id })
      .select("role_id")
      .all()
  ).map(({ role_id }) => role_id);
  return {
    isAdminRole: isAdmin || member.roles.cache.hasAny(...modRoles),
    modRoles,
  };
}
