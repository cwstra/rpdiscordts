import { anyOf } from "@databases/pg-typed";
import {
  checkForGuildAndMember,
  CommandInteractionFromGuild,
} from "../helpers/commands";
import { stringListing } from "../helpers/string";
import { wrappedTask } from "../interaction-wrapper";
import { makeCommand } from "../make-command";
import { User } from "../sql-connections";
import { pipe, tuple } from "fp-ts/function";
import * as E from "fp-ts/Either";
import * as T from "fp-ts/Task";
import * as TE from "fp-ts/TaskEither";
import Characters from "../generated_schema/user_data/characters";
import { partitionMap } from "fp-ts/lib/Array";
import { sanitizeTask } from "../helpers/task";
import { GuildMember, Permissions } from "discord.js";

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
            "Zero or more `--Name Value` pairs. If Name or Value contains spaces, quotation marks are necessary.",
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
        name: {
          type: "string",
          description: "The name of the character.",
          required: true,
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
        },
        attributes: {
          type: "string",
          description:
            "A space separated list of `--Name Attribute` pairs or lone `--Name`s.",
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
        },
      },
    },
  },
  execute: wrappedTask((args) => {
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
              // TODO: Replace with TE.fromNullable when that's available
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
                const entries = Object.entries(attributes).filter(
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
                                ? "the following attributes:\n${tripleBackTick}\n" +
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
            const { name, new_name } = options.options;
            return pipe(
              getExistingCharacter({ name, interaction }),
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
                      `Hm. Something went wrong while renaming \`${name}\` to \`${new_name}\`. Maybe try again?`
                  ),
                  TE.chain(() =>
                    TE.left(`Character \`${name}\` renamed to \`${new_name}\`!`)
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
});
function getExistingCharacter({
  name,
  interaction,
}: {
  name: string;
  interaction: CommandInteractionFromGuild;
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
function adminGuard(interaction: CommandInteractionFromGuild) {
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
function parseGetAttrArgs(attributeString: string | null) {
  if (!attributeString) return [];
  // We can safely purge this character from the attributes at
  // the start, and then use it as a marker for quoted groups
  const nullChar = "\u0000";
  const quoteRegex = /"((?:[^"\\]|\\.)*)"/;
  const splitOnQuotes = attributeString.replace(nullChar, "").split(quoteRegex);
  const quotedSections = splitOnQuotes.filter((_, i) => i % 2);
  const dequotedString = splitOnQuotes
    .map((s, i) => (i % 2 ? `${nullChar}${(i - 1) / 2}${nullChar}` : s))
    .join("");
  const lookup = (s: string) =>
    s[0] === nullChar && s[0] === s.slice(-1)
      ? quotedSections[parseInt(s.slice(1, -1))]
      : s;
  const entries = dequotedString.split(/\s+/);
  return entries.map((e) => lookup(e));
}
function parseSetAttrArgs(attributeString: string | null) {
  if (!attributeString) return {};
  // We can safely purge this character from the attributes at
  // the start, and then use it as a marker for quoted groups
  const nullChar = "\u0000";
  const quoteRegex = /"((?:[^"\\]|\\.)*)"/;
  const splitOnQuotes = attributeString.replace(nullChar, "").split(quoteRegex);
  const quotedSections = splitOnQuotes.filter((_, i) => i % 2);
  const dequotedString = splitOnQuotes
    .map((s, i) => (i % 2 ? `${nullChar}${(i - 1) / 2}${nullChar}` : s))
    .join("");
  const lookup = (s: string) =>
    s[0] === nullChar && s[0] === s.slice(-1)
      ? quotedSections[parseInt(s.slice(1, -1))]
      : s;
  const entries = dequotedString.split(/\s+/);
  const { left: toDelete, right: toUpsert } = pipe(
    entries,
    partitionMap((e) => {
      const eqInd = e.indexOf("=");
      const [k, v] =
        eqInd > -1 ? [e.slice(0, eqInd), e.slice(eqInd + 1)] : [e, null];
      return v === null ? E.left(lookup(k)) : E.right(tuple(lookup(k), v));
    })
  );
  return { toUpsert, toDelete };
}

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
