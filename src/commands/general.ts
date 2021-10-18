import { Collection } from "discord.js";
import {
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,
  BOT_GIT_URL,
  DONATION_TEXT,
} from "../env-vars";
import { compact } from "../helpers";
import { makeCommand } from "../make-command";

const phrases = ["Hey!", "Listen!", "Watch out!"];

class Fairy {
  public index = 0;
  getWord() {
    const phrase = phrases[this.index];
    this.index = (this.index + 1) % phrases.length;
    return phrase;
  }
}

const serverFairies = new Collection<string, Fairy>();

module.exports = {
  commands: compact([
    BOT_DISCORD_SERVER &&
      makeCommand({
        name: "server_link",
        description: "Sends the link to the bot discord server.",
        async execute(interaction) {
          await interaction.reply(BOT_DISCORD_SERVER!);
        },
      }),
    BOT_DOC_URL &&
      makeCommand({
        name: "docs",
        description: "Sends the link to the bot's online documentation",
        async execute(interaction) {
          await interaction.reply(BOT_DOC_URL!);
        },
      }),
    BOT_GIT_URL &&
      makeCommand({
        name: "docs",
        description: "Sends the link to the bot's github repository",
        async execute(interaction) {
          await interaction.reply(BOT_GIT_URL!);
        },
      }),
    DONATION_TEXT &&
      makeCommand({
        name: "docs",
        description: "Sends links to donate to the bot's upkeep",
        async execute(interaction) {
          await interaction.reply(DONATION_TEXT!);
        },
      }),
    makeCommand({
      name: "invite",
      description: "Sends a link to invite the bot to other servers.",
      async execute(interaction) {
        await interaction.reply(
          `https://discord.com/api/oauth2/authorize?client_id=${interaction.applicationId}&permissions=0&scope=bot%20applications.commands`
        );
      },
    }),
    makeCommand({
      name: "ping",
      description: "Calculates ping time.",
      async execute(interaction) {
        const sent = await interaction.reply({
          content: "Pinging...",
          fetchReply: true,
        });
        if ("createdTimestamp" in sent)
          interaction.editReply(
            `Pong! Latency: ${
              sent.createdTimestamp - interaction.createdTimestamp
            }ms`
          );
        else
          interaction.editReply(
            `Hm. Didn't get a timestamp when I tried to ping.`
          );
      },
    }),
    makeCommand({
      name: "poke",
      description:
        "Pokes the bot, mostly to see if its online. Use `ping` to get a response time.",
      async execute(interaction) {
        if (!interaction.guildId) {
          return;
        }
        let fairy = serverFairies.get(interaction.guildId);
        if (!fairy) {
          fairy = new Fairy();
          serverFairies.set(interaction.guildId, fairy);
        }
        await interaction.reply(fairy.getWord());
      },
    }),
  ]),
};
