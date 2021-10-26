import {
  CommandInteraction,
  GuildMember,
  MessageSelectOptionData,
  Permissions,
} from "discord.js";
import { makeCommand, makeSubCommands } from "../make-command";
import { fetchSharedEntry, User } from "../sql-connections";
import { makeMessageActions } from "../make-message-actions";
import { compact, trace } from "../helpers";
import { interactionWrapper, WrappedReplies } from "../interaction-wrapper";
import { sortBy } from "rambda";
import Servers from "../generated_schema/user_data/servers";
import Channels from "../generated_schema/user_data/channels";

const settingGroup = (target: "server" | "channel") => ({
  description: `Check or change ${target} settings`,
  subCommands: makeSubCommands({
    charsign: {
      description: `Check or change the current ${target} charsign for roll queries`,
      options: {
        charsign: {
          type: "string",
          description: `If given, set the ${target}'s charsign to this symbol. Otherwise, get the ${target}'s current charsign.`,
          required: false,
        },
      },
    },
    charsep: {
      description: `Check or change the current ${target}'s charsep for roll queries`,
      options: {
        charsep: {
          type: "string",
          description: `If given, set the ${target}'s charsep to this symbol. Otherwise, get the ${target}'s current charsep.`,
          required: false,
        },
      },
    },
    codex: {
      description: `Check or change the current ${target}'s lookup codex.`,
    },
    ephemeral: {
      description: `Toggle ephemeral replies in the current ${target}.`,
    },
    freeze_on_close: {
      description: `Toggle paginator freezing in the current ${target}.`,
    },
    inline: {
      description: `Toggle inline rolling in the current ${target}.`,
    },
  }),
});

module.exports = makeCommand({
  name: "settings",
  description: "Check or change bot settings",
  subcommandGroups: {
    server: settingGroup("server"),
    channel: settingGroup("channel"),
  },
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
  },
  async execute(interaction, options) {
    const wrapped = await interactionWrapper(interaction);
    if (!interaction.guild || !interaction.member)
      return await wrapped.reply(
        `Sorry ${interaction.user.username}, this command only works in servers.`
      );
    if (!("guild" in interaction.member))
      return await wrapped.reply(
        `Hm. I didn't get a proper member from that interaction.`
      );
    const { canEditSettings, modRoles } = await roleCheck(
      interaction.member,
      interaction.guild.ownerId,
      options.subCommand === "bot_mod_roles"
    );
    if (!canEditSettings)
      return await wrapped.reply(
        `Sorry ${interaction.user.username}, to use this command, you must either be a server admin, or have a role that's allowed to change this server's settings.`
      );

    switch (options.subCommand) {
      case "bot_mod_roles": {
        const { role } = options.options;
        if (role) {
          if (modRoles.includes(role.id)) {
            await baseYesNoDialog(interaction, wrapped, {
              query: `Would you like to remove bot-changing permissions from the role ${role.name}?`,
              success: `Bot-changing permissions removed from the ${role.name} role!`,
              cancel: "Permissions change cancelled!",
              error:
                "Hm. Something went wrong while removing that role. Maybe try again?",
              action: () =>
                User.tables.server_mod_roles(User.db).delete({
                  server_id: interaction.guild!.id,
                  role_id: role.id,
                }),
            });
          } else {
            await baseYesNoDialog(interaction, wrapped, {
              query: `Would you like to add bot-changing permissions to the role ${role.name}?`,
              success: `Bot-changing permissions added to the ${role.name} role!`,
              cancel: "Permissions change cancelled!",
              error:
                "Hm. Something went wrong while adding that role. Maybe try again?",
              action: async () => {
                await User.tables.server_mod_roles(User.db).insert({
                  server_id: interaction.guild!.id,
                  role_id: role.id,
                });
              },
            });
          }
        } else {
          if (modRoles.length) {
            const roles = (await interaction.guild!.roles.fetch())
              .filter((r) => modRoles.includes(r.id))
              .map((r) => r.name);
            await wrapped.reply(
              [
                "The current roles with permission to alter this bot's settings are:",
                "```",
                ...roles,
                "```",
                "If you want to add another role, or remove an existing one, provide it to this command.",
                "(Admins will always be able to edit these settings.)",
              ].join("\n")
            );
          } else {
            await wrapped.reply(
              [
                "Currently, only admins are allowed to alter this bot's settings",
                "If you want to give a particular role on your server permission to do so, provide it to this command.",
              ].join("\n")
            );
          }
        }
        return;
      }
      case "charsep":
      case "charsign": {
        const target = options.group;
        const { guild, channel } = interaction;
        const symbolArg =
          options.subCommand === "charsign"
            ? options.options.charsign
            : options.options.charsep;
        const [source, currentEntry] = await fetchSharedEntry(
          target,
          guild.id,
          channel?.id
        );
        const currentSymbols = compact(
          options.subCommand === "charsign"
            ? currentEntry?.charsigns
            : currentEntry?.charseps
        );
        await symbolListDialog(interaction, wrapped, {
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
          currentSymbols: currentSymbols.length ? currentSymbols : null,
          defaultSymbol: options.subCommand === "charsign" ? "$" : ":",
          symbolArg,
          symbolName: options.subCommand,
          source,
          target,
        });
        return;
      }
      case "codex": {
        const target = options.group;
        const { guild, channel } = interaction;
        const [, currentEntry] = await fetchSharedEntry(
          target,
          guild.id,
          channel?.id
        );
        const currentCodex = currentEntry?.codex ?? null;
        const codexList = sortBy(
          (c) => c.display_name,
          await User.tables.codex_list(User.db).find().all()
        );
        const message = await wrapped.reply({
          content: (currentCodex
            ? [
                `The current codex for this ${target} is \`${
                  codexList.find((c) => c.id === currentCodex)?.display_name
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
            [{ customId: "cancel", label: "Cancel", style: "PRIMARY" }],
          ]),
          fetchReply: true,
        });
        if (!("channel_id" in message)) {
          try {
            const response = await message.awaitMessageComponent({
              filter: (i) => {
                i.deferUpdate();
                return i.user.id === interaction.user.id;
              },
              time: 15000,
            });
            if (response.componentType === "BUTTON") {
              await wrapped.editReply({
                content: "Codex change cancelled!",
                components: [],
              });
            } else if (response.isSelectMenu()) {
              try {
                const newCodexId = parseInt(response.values[0]);
                if (target === "server")
                  await User.tables
                    .servers(User.db)
                    .insertOrUpdate(["server_id"], {
                      server_id: guild.id,
                      codex: newCodexId,
                    });
                await wrapped.editReply({
                  content: `Codex changed to \`${
                    codexList.find((c) => c.id === newCodexId)?.display_name
                  }\`!`,
                  components: [],
                });
              } catch (e) {
                await wrapped.editReply({
                  content:
                    "Hm. Something went wrong while changing the codex. Maybe try again?",
                  components: [],
                });
              }
            }
          } catch (e) {
            await wrapped.editReply({
              content: "Response timed out.",
              components: [],
            });
          }
        }
        return;
      }
      case "freeze_on_close": {
        const target = options.group;
        const { guild, channel } = interaction;
        const [source, currentEntry] = await fetchSharedEntry(
          target,
          guild.id,
          channel?.id
        );
        const freeze_on_timeout = currentEntry?.freeze_on_timeout ?? false;
        await baseYesNoDialog(interaction, wrapped, {
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
        return;
      }
      case "inline": {
        const target = options.group;
        const { guild, channel } = interaction;
        const [source, currentEntry] = await fetchSharedEntry(
          target,
          guild.id,
          channel?.id
        );
        const inline = currentEntry?.inline ?? false;
        await baseYesNoDialog(interaction, wrapped, {
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
        return;
      }
      case "ephemeral": {
        const target = options.group;
        const { guild, channel } = interaction;
        const [source, currentEntry] = await fetchSharedEntry(
          target,
          guild.id,
          channel?.id
        );
        const ephemeral = currentEntry?.ephemeral ?? true;
        await baseYesNoDialog(interaction, wrapped, {
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
        return;
      }
      default: {
        const e: never = options;
        return e;
      }
    }
  },
});

async function roleCheck(
  member: GuildMember,
  ownerId: string,
  isForRoleCheck: boolean
): Promise<{ canEditSettings: boolean; modRoles: string[] }> {
  const isAdmin =
    member.id === ownerId &&
    member.permissions.has(Permissions.FLAGS.ADMINISTRATOR);
  if (isAdmin && !isForRoleCheck)
    return { canEditSettings: true, modRoles: [] };
  const modRoles = (
    await User.tables
      .server_mod_roles(User.db)
      .find({ server_id: member.guild.id })
      .select("role_id")
      .all()
  ).map(({ role_id }) => role_id);
  return {
    canEditSettings: isAdmin || member.roles.cache.hasAny(...modRoles),
    modRoles,
  };
}

type BaseYesNoDialogArgs = {
  query: string;
  success: string;
  cancel: string;
  error: string;
  action: () => Promise<void>;
};

async function baseYesNoDialog(
  interaction: CommandInteraction,
  wrapped: WrappedReplies,
  { query, success, cancel, error, action }: BaseYesNoDialogArgs
) {
  const message = await wrapped.reply(
    trace({
      content: query,
      fetchReply: true,
      components: makeMessageActions([
        [
          { customId: "yes", label: "Yes", style: "PRIMARY" },
          { customId: "no", label: "No", style: "PRIMARY" },
        ],
      ]),
    })
  );
  // Assuming this is a versioning relic for now.
  // We'll deal with the API logic if/when it becomes a problem
  if (!("channel_id" in message)) {
    try {
      const response = await message.awaitMessageComponent({
        filter: async (i) => {
          await i.deferUpdate();
          return i.user.id === interaction.user.id;
        },
        componentType: "BUTTON",
        time: 15000,
      });
      if (response.customId === "yes") {
        try {
          await action();
          await wrapped.editReply({ content: success, components: [] });
        } catch (e) {
          await wrapped.editReply({ content: error, components: [] });
        }
      } else wrapped.editReply({ content: cancel, components: [] });
    } catch (e) {
      await wrapped.editReply({
        content: "Response timed out.",
        components: [],
      });
    }
  }
}

async function symbolListDialog(
  interaction: CommandInteraction,
  wrapped: WrappedReplies,
  {
    action,
    currentSymbols,
    defaultSymbol,
    symbolArg,
    symbolName,
    source,
    target,
  }: {
    action: (arr: string[]) => Promise<void>;
    currentSymbols: string[] | null;
    defaultSymbol: string;
    symbolArg: string | null;
    symbolName: string;
    source: "channel" | "server";
    target: "channel" | "server";
  }
) {
  const capitalSymbolName = symbolName.replace(/^\w/, (s) => s.toUpperCase());
  if (symbolArg) {
    if (
      currentSymbols
        ? currentSymbols.length === 1 && currentSymbols[0] === symbolArg
        : symbolArg === defaultSymbol
    ) {
      await wrapped.reply(
        [
          `Sorry, I can't remove this ${target}'s only charsep.`,
          "Add another one if you want to get rid of it.",
        ].join("\n")
      );
      return;
    }
    const isForRemoval = currentSymbols && currentSymbols.includes(symbolArg);
    const newValue = isForRemoval
      ? currentSymbols.filter((s) => s !== symbolArg)
      : [...(currentSymbols ?? [defaultSymbol]), symbolArg];
    await baseYesNoDialog(interaction, wrapped, {
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
      success: `${capitalSymbolName} ${isForRemoval ? "removed" : "added"}!`,
      cancel: `${capitalSymbolName} change cancelled!`,
      error: `Hm. Something went wrong while ${
        isForRemoval ? "removing" : "adding"
      } the ${symbolName}. Maybe try again?`,
      action: () => action(newValue),
    });
  } else {
    if (
      !currentSymbols ||
      (currentSymbols.length === 1 && currentSymbols[0] === defaultSymbol)
    ) {
      await wrapped.reply(
        [
          `This ${target} currently uses the default ${symbolName}: \`${defaultSymbol}\`.`,
          `If you would like to add another ${symbolName}, pass it to this command.`,
        ].join("\n")
      );
    } else {
      await wrapped.reply(
        [
          `This ${target} currently uses the following ${symbolName}s:`,
          "```",
          ...currentSymbols,
          "```",
          `If you would like to add another ${symbolName}, or remove one of the existing ones, pass it to this command.`,
        ].join("\n")
      );
    }
  }
}

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
