import { join } from "path";
import { SlashCommandBuilder } from "@discordjs/builders";
import { CommandInteraction } from "discord.js";
import * as fs from "fs";

const commandDir = join(__dirname, "commands");

export const commandList = fs
  .readdirSync(commandDir)
  .filter((file) => file.endsWith(".js"))
  .flatMap(
    (
      file
    ): {
      data: SlashCommandBuilder;
      execute: (interaction: CommandInteraction) => Promise<void>;
    }[] => {
      console.log(`Loading ${join("commands", file)}`);
      const commandOrCommands = require(join(commandDir, file));
      return "commands" in commandOrCommands
        ? commandOrCommands["commands"]
        : [commandOrCommands];
    }
  );
