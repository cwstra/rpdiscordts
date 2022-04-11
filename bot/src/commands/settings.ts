import {
  CommandInteraction,
  MessageSelectOptionData,
  Permissions,
} from "discord.js";
import { makeCommand } from "../make-command";
import { fetchSharedEntry, User } from "../sql-connections";
import { makeMessageActions } from "../make-message-actions";
import {
  checkForGuildAndMember,
  CommandInteractionFromGuild,
} from "../helpers/commands";
import { assert, checkChannelSendPerms, trace } from "../helpers/general";
import { compact, findApply } from "../helpers/array";
import { WrappedReplies, wrappedTask } from "../interaction-wrapper";
import { equals, sortBy } from "rambda";
import Servers from "../generated_schema/user_data/servers";
import Channels from "../generated_schema/user_data/channels";
import { pipe } from "fp-ts/function";
import * as M from "fp-ts/Map";
import * as O from "fp-ts/Option";
import * as S from "fp-ts/string";
import * as T from "fp-ts/Task";
import * as TO from "fp-ts/TaskOption";
import * as TE from "fp-ts/TaskEither";
import { evolve } from "fp-ts/lib/struct";
import { ensureTask } from "../helpers/task";

const targetOption = {
  type: "string" as const,
  description: `The target of the settings modification.`,
  required: true as const,
  choices: [
    ["Channel", "channel"],
    ["Server", "server"],
  ] as [name: string, value: "channel" | "server"][],
};

module.exports = makeCommand({
  name: "settings",
  description: "Check or change bot settings",
  subcommands: {
    bot_mod_roles: {
      description:
        "Get or check the roles that can change the bot's settings in this server.",
      options: {
        role: {
          type: "role",
          description: "Optional. Role to add or remove.",
          required: false,
        },
      },
    },
    charsign: {
      description: "Check or change the charsign for roll queries",
      options: {
        target: targetOption,
        charsign: {
          type: "string",
          description:
            "If given, set the target's charsign to this symbol. Otherwise, get the target's current charsign.",
          required: false,
        },
      },
    },
    charsep: {
      description: "Check or change the charsep for roll queries",
      options: {
        target: targetOption,
        charsep: {
          type: "string",
          description:
            "If given, set the target's charsep to this symbol. Otherwise, get the target's current charsep.",
          required: false,
        },
      },
    },
    codex: {
      description: "Check or change the lookup codex.",
      options: {
        target: targetOption,
      },
    },
    ephemeral: {
      description: "Toggle ephemeral replies.",
      options: {
        target: targetOption,
      },
    },
    freeze_on_close: {
      description: "Toggle paginator freezing.",
      options: {
        target: targetOption,
      },
    },
  },
  execute: wrappedTask((args) =>
    pipe(
      args,
      checkForGuildAndMember,
      checkForAdminRole(args.options.subCommand === "bot_mod_roles"),
      TE.chain(
        ({
          interaction,
          wrapped,
          modRoles,
          options,
        }): TE.TaskEither<string, void> => {
          const { subCommand } = options;
          switch (subCommand) {
            case "bot_mod_roles": {
              const { role } = options.options;
              if (role) {
                const isForRemove = modRoles.includes(role.id);
                return TE.fromTask(
                  doBaseYesNoDialog(interaction, wrapped, {
                    query: `Would you like to ${
                      isForRemove ? "remove" : "add"
                    } bot-changing permissions ${
                      isForRemove ? "from" : "to"
                    } the role ${role.name}?`,
                    success: `Bot-changing permissions ${
                      isForRemove ? "removed from" : "added to"
                    } the ${role.name} role!`,
                    cancel: "Permissions change cancelled!",
                    error: `Hm. Something went wrong while ${
                      isForRemove ? "adding" : "removing"
                    } that role. Maybe try again?`,
                    action: isForRemove
                      ? () =>
                          User.tables.server_mod_roles(User.db).delete({
                            server_id: interaction.guild!.id,
                            role_id: role.id,
                          })
                      : async () => {
                          await User.tables.server_mod_roles(User.db).insert({
                            server_id: interaction.guild!.id,
                            role_id: role.id,
                          });
                        },
                  })
                );
              } else {
                return TE.leftTask(async () => {
                  if (modRoles.length) {
                    return pipe(
                      await interaction.guild!.roles.fetch(),
                      M.filterMap((r) =>
                        modRoles.includes(r.id) ? O.some(r.name) : O.none
                      ),
                      M.reduce(S.Ord)("", (acc, name) => `${acc}\n${name}`),
                      (roleString) =>
                        [
                          "The current roles with permission to alter this bot's settings are:",
                          "```",
                          ...roleString,
                          "```",
                          "If you want to add another role, or remove an existing one, provide it to this command.",
                          "(Admins will always be able to edit these settings.)",
                        ].join("\n")
                    );
                  } else {
                    return [
                      "Currently, only admins are allowed to alter this bot's settings",
                      "If you want to give a particular role on your server permission to do so, provide it to this command.",
                    ].join("\n");
                  }
                });
              }
            }
            case "charsep":
            case "charsign": {
              const target = options.options.target;
              const { guild, channel } = interaction;
              const opposite =
                subCommand === "charsign" ? "charsep" : "charsign";
              // TODO: Add check that existing characters don't use the provided symbol.
              const symbolArg =
                subCommand === "charsign"
                  ? options.options.charsign
                  : options.options.charsep;
              return pipe(
                () =>
                  // TODO: Resolve this so we don't have weird server/channel interactions
                  fetchSharedEntry({
                    target,
                    server_id: guild.id,
                    channel_id: channel?.id,
                  }),
                T.chain(([source, currentEntry]) => {
                  const [currentSymbols, otherSymbols] = [
                    currentEntry?.[`${subCommand}s`],
                    currentEntry?.[`${opposite}s`],
                  ].map(compact);
                  const forbidden = (newS: string) => {
                    const findBad = (listName: string, l: string[]) =>
                      findApply(l, (s) =>
                        s.includes(newS)
                          ? {
                              listName,
                              relation: "is currently contained in",
                              value: s,
                            }
                          : newS.includes(s) && {
                              listName,
                              relation: "currently contains",
                              value: s,
                            }
                      );
                    const badCurrent =
                      findBad(subCommand, currentSymbols) ||
                      findBad(opposite, otherSymbols);
                    if (badCurrent) {
                      const { listName, relation, value } = badCurrent;
                      return [
                        `Sorry, \`${newS}\` ${relation} the current ${listName} \`${value}\`.`,
                        `You'll have to get rid of that ${listName} to add this one.`,
                      ].join("\n");
                    }
                  };
                  return doSymbolListDialog(interaction, wrapped, {
                    action: sharedEntryUpdate<[string[]]>({
                      server_id: interaction.guild!.id,
                      channel_id: interaction.channel!.id,
                      currentEntry,
                      source,
                      target,
                      updateObject:
                        options.subCommand === "charsep"
                          ? (charseps) => ({ charseps })
                          : (charsigns) => ({ charsigns }),
                    }),
                    currentSymbols: currentSymbols.length
                      ? currentSymbols
                      : null,
                    defaultSymbol:
                      options.subCommand === "charsign" ? "$" : ":",
                    forbidden,
                    symbolArg,
                    symbolName: options.subCommand,
                    source,
                    target,
                  });
                }),
                (t) => TE.fromTask(t)
              );
            }
            case "codex": {
              const target = options.options.target;
              const { guild, channel } = interaction;
              return pipe(
                () =>
                  fetchSharedEntry({
                    target,
                    server_id: guild.id,
                    channel_id: channel?.id,
                    item: "codex",
                  }),
                T.chain(([source, currentEntry, codex]) =>
                  pipe(
                    () => User.tables.codex_list(User.db).find().all(),
                    T.map((users) => ({
                      codexList: sortBy((c) => c.display_name, users),
                      currentCodex: codex ?? null,
                      currentEntry,
                      source,
                    }))
                  )
                ),
                T.bind(
                  "message",
                  ({ codexList, currentCodex }) =>
                    () =>
                      wrapped.reply({
                        content: (currentCodex
                          ? [
                              `The current codex for this ${target} is \`${
                                codexList.find((c) => c.id === currentCodex)
                                  ?.display_name
                              }\``,
                              "You can use the select menu below to change it to another one.",
                            ]
                          : [
                              `The current ${target} has no codex.`,
                              "You can use the select menu below to pick one.",
                            ]
                        ).join("\n"),
                        components: makeMessageActions([
                          {
                            customId: "codex-select",
                            // TODO: If we get more than 25 codexes, remove the assertion
                            //       and paginate somehow.
                            options: codexList.map(
                              (c): MessageSelectOptionData => ({
                                label: c.display_name,
                                value: `${c.id}`,
                              })
                            ) as any,
                          },
                          [
                            {
                              customId: "cancel",
                              label: "Cancel",
                              style: "PRIMARY",
                            },
                          ],
                        ]),
                        fetchReply: true,
                      })
                ),
                T.map(
                  evolve({
                    codexList: (a) => a,
                    currentCodex: (a) => a,
                    currentEntry: (a) => a,
                    message: (message) => {
                      assert(!("channel_id" in message));
                      return message;
                    },
                    source: (a) => a,
                  })
                ),
                T.chain(({ codexList, currentEntry, message, source }) =>
                  pipe(
                    () =>
                      message.awaitMessageComponent({
                        filter: (i) => {
                          i.deferUpdate();
                          return i.user.id === interaction.user.id;
                        },
                        time: 15000,
                      }),
                    T.chain((response) => {
                      if (response.componentType === "BUTTON")
                        return T.of("Codex change cancelled!");
                      assert(response.isSelectMenu());
                      try {
                        return pipe(
                          T.of(parseInt(response.values[0])),
                          T.chainFirst(
                            (a) => () =>
                              sharedEntryUpdate<[number | null | undefined]>({
                                server_id: interaction.guild.id,
                                channel_id: interaction.channel!.id,
                                currentEntry,
                                source,
                                target,
                                updateObject: (codex) => ({ codex }),
                              })(a)
                          ),
                          T.map(
                            (codexId) =>
                              `Codex changed to \`${
                                codexList.find((c) => c.id === codexId)
                                  ?.display_name
                              }\`!`
                          )
                        );
                      } catch (e) {
                        return T.of(
                          "Hm. Something went wrong while changing the codex. Maybe try again?"
                        );
                      }
                    }),
                    ensureTask(() => "Response timed out."),
                    T.chain(
                      (content) => () =>
                        wrapped.editReply({ content, components: [] })
                    )
                  )
                ),
                T.map(() => {}),
                (t) => TE.fromTask(t)
              );
            }
            case "freeze_on_close": {
              const target = options.options.target;
              const { guild, channel } = interaction;
              return pipe(
                () =>
                  fetchSharedEntry({
                    target,
                    server_id: guild.id,
                    channel_id: channel?.id,
                    item: "freeze_on_timeout",
                  }),
                T.chain(([source, currentEntry, freeze_on_timeout = false]) => {
                  return doBaseYesNoDialog(interaction, wrapped, {
                    query: [
                      `Currently, the bot will ${
                        freeze_on_timeout ? "freeze" : "delete"
                      } paginators in this ${target} when it stops listening to them.`,
                      `Would you like it to ${
                        freeze_on_timeout ? "delete" : "freeze"
                      } them instead?`,
                    ].join("\n"),
                    success: `Paginators will now be ${
                      freeze_on_timeout ? "deleted" : "frozen"
                    } on timeout!`,
                    cancel: `Paginator behavior change cancelled!`,
                    error: `Hm. Something went wrong while changing paginator behavior. Maybe try again?`,
                    action: sharedEntryUpdate({
                      server_id: interaction.guild!.id,
                      channel_id: interaction.channel!.id,
                      currentEntry,
                      source,
                      target,
                      updateObject: () => ({
                        freeze_on_timeout: !freeze_on_timeout,
                      }),
                    }),
                  });
                }),
                (t) => TE.fromTask(t)
              );
            }
            /*
            case "inline": {
              const target = options.group;
              const { guild, channel } = interaction;
              return pipe(
                () => fetchSharedEntry(target, guild.id, channel?.id),
                T.chain(([source, currentEntry]) => {
                  const inline = currentEntry?.inline ?? false;
                  return doBaseYesNoDialog(interaction, wrapped, {
                    query: [
                      `The bot ${
                        inline ? "will currently" : "can"
                      } listen to messages in this ${target} for dice expressions, and reply to them with roll results.`,
                      `Would you like to ${
                        inline ? "dis" : "en"
                      }able that functionality?`,
                    ].join("\n"),
                    success: `Inline rolling ${inline ? "dis" : "en"}abled!`,
                    cancel: `Inline rolling change cancelled!`,
                    error: `Hm. Something went wrong while ${
                      inline ? "dis" : "en"
                    }abling inline rolling. Maybe try again?`,
                    action: sharedEntryUpdate({
                      server_id: interaction.guild!.id,
                      channel_id: interaction.channel!.id,
                      currentEntry,
                      source,
                      target,
                      updateObject: () => ({ inline: !inline }),
                    }),
                  });
                }),
                (t) => TE.fromTask(t)
              );
            }*/
            case "ephemeral": {
              const target = options.options.target;
              const { guild, channel } = interaction;
              return pipe(
                () =>
                  fetchSharedEntry({
                    target,
                    server_id: guild.id,
                    channel_id: channel?.id,
                    item: "ephemeral",
                  }),
                T.chain(
                  ([
                    source,
                    currentEntry,
                    ephemeral = !checkChannelSendPerms(interaction),
                  ]) => {
                    return doBaseYesNoDialog(interaction, wrapped, {
                      query: ephemeral
                        ? [
                            `The bot's replies to commands in this ${target} will currently be ephemeral (only visible to the user of the command).`,
                            `Would you like to make them visible to everyone?`,
                          ].join("\n")
                        : [
                            `The bot's replies to commands in this ${target} will currently be visible to everyone.`,
                            `Would you like to make them ephemerial (only visible to the user of the command)?`,
                          ].join("\n"),
                      success: `Commands will now be ${
                        ephemeral ? "visible to everyone" : "ephemeral"
                      }!`,
                      cancel: `Command visibility change cancelled!`,
                      error: `Hm. Something went wrong while changing command visibility. Maybe try again?`,
                      action: sharedEntryUpdate({
                        server_id: interaction.guild!.id,
                        channel_id: interaction.channel!.id,
                        currentEntry,
                        source,
                        target,
                        updateObject: () => ({ ephemeral: !ephemeral }),
                      }),
                    });
                  }
                ),
                (t) => TE.fromTask(t)
              );
            }
            default: {
              const e: never = options;
              return e;
            }
          }
        }
      ),
      TE.getOrElse((e) => () => args.wrapped.reply(e))
    )
  ),
});

const checkForAdminRole = <
  Scope extends { interaction: CommandInteractionFromGuild }
>(
  isForRoleChange?: boolean
) =>
  TE.chain((scope: Scope) => {
    const {
      interaction: {
        user,
        member,
        guild: { ownerId },
      },
    } = scope;
    const isAdmin =
      member.id === ownerId &&
      member.permissions.has(Permissions.FLAGS.ADMINISTRATOR);
    return pipe(
      isAdmin && !isForRoleChange
        ? TO.some({ ...scope, modRoles: [] })
        : pipe(
            () =>
              User.tables
                .server_mod_roles(User.db)
                .find({ server_id: member.guild.id })
                .select("role_id")
                .all(),
            T.map((entries) => {
              const modRoles = entries.map(({ role_id }) => role_id as string);
              return isAdmin || member.roles.cache.hasAny(...modRoles)
                ? O.some({ ...scope, modRoles })
                : O.none;
            })
          ),
      TE.fromTaskOption(
        () =>
          `Sorry ${user.username}, to use this command, you must either be a server admin, or have a role that's allowed to change this server's settings.`
      )
    );
  });

function sharedEntryUpdate<Args extends unknown[] = []>({
  server_id,
  channel_id,
  currentEntry,
  source,
  target,
  updateObject,
}: {
  server_id: string;
  channel_id: string;
  currentEntry: Servers | Channels | null;
  source: "server" | "channel";
  target: "server" | "channel";
  updateObject: (
    ...args: Args
  ) => Partial<
    Pick<Servers, Exclude<keyof Servers & keyof Channels, "server_id">>
  >;
}) {
  if (target === "server")
    return async (...args: Args) => {
      await User.tables
        .servers(User.db)
        .insertOrUpdate(["server_id"], { server_id, ...updateObject(...args) });
    };
  if (source === "server" && currentEntry)
    return async (...args: Args) => {
      await User.tables.channels(User.db).insert({
        ...currentEntry,
        server_id,
        channel_id,
        ...updateObject(...args),
      });
    };
  return async (...args: Args) => {
    await User.tables
      .channels(User.db)
      .insertOrUpdate(["server_id", "channel_id"], {
        server_id,
        channel_id,
        ...updateObject(...args),
      });
  };
}

type BaseYesNoDialogArgs = {
  query: string;
  success: string;
  cancel: string;
  error: string;
  action: () => Promise<void>;
};

function doBaseYesNoDialog(
  interaction: CommandInteraction,
  wrapped: WrappedReplies,
  { query, success, cancel, error, action }: BaseYesNoDialogArgs
) {
  return pipe(
    () =>
      wrapped.reply({
        content: trace(query),
        fetchReply: true,
        components: makeMessageActions([
          [
            { customId: "yes", label: "Yes", style: "PRIMARY" },
            { customId: "no", label: "No", style: "PRIMARY" },
          ],
        ]),
      }),
    T.map((message) => {
      // Assuming this is a versioning relic for now.
      // We'll deal with the API logic if/when it becomes a problem
      assert(!("channel_id" in message));
      return message;
    }),
    T.chain((message) =>
      pipe(
        TE.tryCatch(
          () =>
            message.awaitMessageComponent({
              filter: async (i) => {
                await i.deferUpdate();
                return i.user.id === interaction.user.id;
              },
              componentType: "BUTTON",
              time: 15000,
            }),
          () => "Response timed out."
        ),
        TE.chain(({ customId }) =>
          customId === "yes"
            ? TE.tryCatch(
                async () => {
                  await action();
                  return success;
                },
                () => error
              )
            : TE.of(cancel)
        ),
        TE.toUnion
      )
    ),
    T.chain((s) => async () => {
      wrapped.editReply({ content: s, components: [] });
    })
  );
}

const doSymbolListDialog = (
  interaction: CommandInteraction,
  wrapped: WrappedReplies,
  {
    action,
    currentSymbols,
    defaultSymbol,
    forbidden,
    symbolArg,
    symbolName,
    source,
    target,
  }: {
    action: (arr: string[]) => Promise<void>;
    currentSymbols: string[] | null;
    defaultSymbol: string;
    forbidden?: (s: string) => string | undefined;
    symbolArg: string | null;
    symbolName: string;
    source: "channel" | "server";
    target: "channel" | "server";
  }
) => {
  const capitalSymbolName = symbolName.replace(/^\w/, (s) => s.toUpperCase());
  const defaultSymbolList = [defaultSymbol];
  const isNotDefault = (list: string[] | null): list is string[] =>
    !equals(list ?? defaultSymbolList, defaultSymbolList);
  return pipe(
    symbolArg,
    TE.fromPredicate(
      (s): s is string => !!s,
      () =>
        isNotDefault(currentSymbols)
          ? [
              `This ${target} currently uses the following ${symbolName}s:`,
              "```",
              ...currentSymbols,
              "```",
              `If you would like to add another ${symbolName}, or remove one of the existing ones, pass it to this command.`,
            ].join("\n")
          : [
              `This ${target} currently uses the default ${symbolName}: \`${defaultSymbol}\`.`,
              `If you would like to add another ${symbolName}, pass it to this command.`,
            ].join("\n")
    ),
    TE.filterOrElse(
      (s) => !equals([s], currentSymbols ?? [defaultSymbol]),
      () =>
        [
          `Sorry, I can't remove this ${target}'s only ${symbolName}.`,
          "Add another one if you want to get rid of it.",
        ].join("\n")
    ),
    TE.chain((symbolArg) => {
      const isForRemoval = currentSymbols && currentSymbols.includes(symbolArg);
      const forbiddenMsg = !isForRemoval && forbidden?.(symbolArg);
      if (forbiddenMsg) return TE.left(forbiddenMsg);
      const newValue = isForRemoval
        ? currentSymbols.filter((s) => s !== symbolArg)
        : [...(currentSymbols ?? [defaultSymbol]), symbolArg];
      return TE.fromTask(
        doBaseYesNoDialog(interaction, wrapped, {
          query: compact([
            `Would you like to ${
              isForRemoval ? "remove" : "add"
            } \`${symbolArg}\` ${
              isForRemoval ? "from" : "to"
            } this ${target}'s ${symbolName} list?`,
            !currentSymbols
              ? `This ${target} is currently using the default ${symbolName} of \`${defaultSymbol}\`, so doing so will create a new entry for this ${target}.`
              : target !== source &&
                "This channel is currently using the server defaults, so doing so will create a new entry for this channel specifically.",
          ]).join("\n"),
          success: `${capitalSymbolName} ${
            isForRemoval ? "removed" : "added"
          }!`,
          cancel: `${capitalSymbolName} change cancelled!`,
          error: `Hm. Something went wrong while ${
            isForRemoval ? "removing" : "adding"
          } the ${symbolName}. Maybe try again?`,
          action: () => action(newValue),
        })
      );
    }),
    TE.getOrElse((e) => () => wrapped.reply(e)),
    T.map(() => {})
  );
};
