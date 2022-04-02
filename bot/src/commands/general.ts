import { Collection } from "discord.js";
import {
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,
  BOT_GIT_URL,
  DONATION_TEXT,
} from "../env-vars";
import { compact } from "../helpers/array";
import { wrappedExecute } from "../interaction-wrapper";
import { makeCommand } from "../make-command";

const phrases = ["Hey!", "Listen!", "Watch out!"];

const serverFairies = new Collection<string, number>();

module.exports = {
  commands: compact([
    BOT_DISCORD_SERVER &&
      makeCommand({
        name: "server_link",
        description: "Sends the link to the bot discord server.",
        execute: wrappedExecute(async ({ wrapped }) => {
          await wrapped.reply(BOT_DISCORD_SERVER!);
        }),
      }),
    BOT_DOC_URL &&
      makeCommand({
        name: "docs",
        description: "Sends the link to the bot's online documentation",
        execute: wrappedExecute(async ({ wrapped }) => {
          await wrapped.reply(BOT_DOC_URL!);
        }),
      }),
    BOT_GIT_URL &&
      makeCommand({
        name: "docs",
        description: "Sends the link to the bot's github repository",
        execute: wrappedExecute(async ({ wrapped }) => {
          await wrapped.reply(BOT_GIT_URL!);
        }),
      }),
    DONATION_TEXT &&
      makeCommand({
        name: "docs",
        description: "Sends links to donate to the bot's upkeep",
        execute: wrappedExecute(async ({ wrapped }) => {
          await wrapped.reply(DONATION_TEXT!);
        }),
      }),
    makeCommand({
      name: "invite",
      description: "Sends a link to invite the bot to other servers.",
      execute: wrappedExecute(async ({ interaction, wrapped }) => {
        await wrapped.reply(
          `https://discord.com/api/oauth2/authorize?client_id=${interaction.applicationId}&permissions=0&scope=bot%20applications.commands`
        );
      }),
    }),
    makeCommand({
      name: "ping",
      description: "Calculates ping time.",
      execute: wrappedExecute(async ({ interaction, wrapped }) => {
        const sent = await wrapped.reply({
          content: "Pinging...",
          fetchReply: true,
        });
        if ("createdTimestamp" in sent)
          wrapped.editReply(
            `Pong! Latency: ${
              sent.createdTimestamp - interaction.createdTimestamp
            }ms`
          );
        else
          wrapped.editReply(`Hm. Didn't get a timestamp when I tried to ping.`);
      }),
    }),
    makeCommand({
      name: "poke",
      description:
        "Pokes the bot, mostly to see if its online. Use `ping` to get a response time.",
      execute: wrappedExecute(async ({ interaction, wrapped }) => {
        if (!interaction.guildId) return;
        const phraseIndex = serverFairies.get(interaction.guildId) ?? 0;
        await wrapped.reply(phrases[phraseIndex]);
        serverFairies.set(
          interaction.guildId,
          (phraseIndex + 1) % phrases.length
        );
      }),
    }),
  ]),
};
