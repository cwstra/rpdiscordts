import { ChannelType, CommandInteraction, PermissionsBitField } from "discord.js";
import { FlattenedArrayUnion, FlattenedObjectUnion } from "./types";

export function assert(condition: any, msg?: string): asserts condition {
  if (!condition) {
    throw new Error(msg);
  }
}

export function typeAssert<Base, Extended extends Base>() {
  return;
}

export function flattenedUnion<ObjUnion extends object>(
  unionMember: ObjUnion
): FlattenedObjectUnion<ObjUnion>;
export function flattenedUnion<ObjUnion extends object>(
  unionMember?: ObjUnion
): FlattenedObjectUnion<ObjUnion> | undefined;
export function flattenedUnion<ArrayUnion extends readonly unknown[]>(
  unionMember: ArrayUnion
): FlattenedArrayUnion<ArrayUnion>;
export function flattenedUnion<ArrayUnion extends readonly unknown[]>(
  unionMember?: ArrayUnion
): FlattenedArrayUnion<ArrayUnion> | undefined;
export function flattenedUnion(unionMember: unknown) {
  return unionMember;
}

export function trace<T>(t: T): T {
  console.log(t);
  return t;
}

export function isKeyOf<T extends object>(t: T) {
  return (s: PropertyKey): s is keyof T => s in t;
}

export function checkChannelSendPerms(interaction: CommandInteraction) {
  switch (interaction.channel?.type) {
    case ChannelType.DM:
      return true;
    case ChannelType.GuildText:
    case ChannelType.GuildAnnouncement: {
      if (!interaction.guild?.members.me) return false;
      const perms = interaction.channel.permissionsFor(interaction.guild.members.me);
      return perms.has(PermissionsBitField.Flags.SendMessages);
    }
    case ChannelType.PublicThread:
    case ChannelType.AnnouncementThread:
    case ChannelType.PrivateThread: {
      if (!interaction.guild?.members.me) return false;
      const perms = interaction.channel.permissionsFor(interaction.guild.members.me);
      return perms.has(PermissionsBitField.Flags.SendMessagesInThreads);
    }
    case null:
    case undefined:
      return false;
  }
}
