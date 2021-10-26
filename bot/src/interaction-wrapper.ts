import { CommandInteraction, MessagePayload } from "discord.js";
import { fetchSharedEntry } from "./sql-connections";

type Options = Parameters<CommandInteraction["reply"]>[0];
export type WrappedReplies = {
  defaultEphemeral: boolean;
  reply: CommandInteraction["reply"];
  editReply: CommandInteraction["editReply"];
};

export async function interactionWrapper(
  interaction: CommandInteraction
): Promise<WrappedReplies> {
  const defaultEphemeral = interaction.guildId
    ? (
        await fetchSharedEntry(
          "channel",
          interaction.guildId,
          interaction.channelId
        )
      )[1]?.ephemeral ?? true
    : false;
  const wrapOptions = defaultEphemeral
    ? (o: Options): Options =>
        typeof o === "string"
          ? { content: o, ephemeral: true }
          : o instanceof MessagePayload
          ? o
          : { ephemeral: true, ...o }
    : (o: Options) => o;
  return {
    defaultEphemeral,
    reply: ((o: Options) =>
      interaction.reply(wrapOptions(o))) as typeof interaction.reply,
    editReply: ((o: Options) =>
      interaction.editReply(wrapOptions(o))) as typeof interaction.editReply,
  };
}
