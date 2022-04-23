import * as TE from "fp-ts/TaskEither";
import { Interaction, Guild, GuildMember } from "discord.js";
import { pipe } from "fp-ts/function";
import { NonNullProps } from "./types";

export type InteractionFromGuild = Interaction & {
  guild: Guild;
  member: GuildMember;
};

export const checkForGuildAndMember = <
  Scope extends { interaction: Interaction }
>(
  scope: Scope
) =>
  pipe(
    scope,
    TE.fromPredicate(
      (
        s: Scope
      ): s is Scope & {
        interaction: NonNullProps<Interaction, "guild" | "member">;
      } => !!(s.interaction.guild && s.interaction.member),
      (s) =>
        `Sorry ${s.interaction.user.username}, this command only works in servers.`
    ),
    TE.filterOrElse(
      (
        s
      ): s is Scope & {
        interaction: InteractionFromGuild;
      } => "guild" in s.interaction.member,
      () => `Hm. I didn't get a proper member from that interaction.`
    )
  );
