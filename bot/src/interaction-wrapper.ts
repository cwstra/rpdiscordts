import * as T from "fp-ts/Task";
import {
  CommandInteraction,
  InteractionDeferReplyOptions,
  MessagePayload,
} from "discord.js";
import { fetchSharedEntry } from "./sql-connections";
import { checkChannelSendPerms } from "./helpers/general";

type Options = Parameters<CommandInteraction["reply"]>[0];
export type WrappedReplies = {
  defaultEphemeral: boolean;
  reply: CommandInteraction["reply"];
  editReply: CommandInteraction["editReply"];
  deferReply: CommandInteraction["deferReply"];
};

export async function interactionWrapper(
  interaction: CommandInteraction
): Promise<WrappedReplies> {
  const defaultEphemeral =
    checkChannelSendPerms(interaction) &&
    (interaction.guildId
      ? (
          await fetchSharedEntry({
            target: "channel",
            server_id: interaction.guildId,
            channel_id: interaction.channelId,
            item: "ephemeral",
          })
        )[2] ?? false
      : false);
  const wrapOptions = defaultEphemeral
    ? (o: Options): Options =>
        typeof o === "string"
          ? { content: o, ephemeral: true }
          : o instanceof MessagePayload
          ? o
          : { ephemeral: true, ...o }
    : (o: Options) => o;
  const wrapDeferOptions = defaultEphemeral
    ? (o: InteractionDeferReplyOptions): InteractionDeferReplyOptions => ({
        ephemeral: true,
        ...o,
      })
    : (o: InteractionDeferReplyOptions) => o;
  return {
    defaultEphemeral,
    reply: ((o: Options) =>
      interaction.reply(wrapOptions(o))) as typeof interaction.reply,
    editReply: ((o: Options) =>
      interaction.editReply(wrapOptions(o))) as typeof interaction.editReply,
    deferReply: ((o: InteractionDeferReplyOptions) =>
      interaction.deferReply(
        wrapDeferOptions(o)
      )) as typeof interaction.deferReply,
  };
}

export const wrappedExecute =
  <Options>(
    exec: (args: {
      interaction: CommandInteraction;
      options: Options;
      wrapped: WrappedReplies;
    }) => Promise<void>
  ) =>
  async (interaction: CommandInteraction, options: Options): Promise<void> => {
    const wrapped = await interactionWrapper(interaction);
    await exec({ interaction, options, wrapped });
  };

export const wrappedTask =
  <Options>(
    task: (args: {
      interaction: CommandInteraction;
      options: Options;
      wrapped: WrappedReplies;
    }) => T.Task<void>
  ) =>
  async (interaction: CommandInteraction, options: Options): Promise<void> => {
    const wrapped = await interactionWrapper(interaction);
    await task({ interaction, options, wrapped })();
  };
