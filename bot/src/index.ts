import { Client, Collection, CommandInteraction, Intents } from "discord.js";
import { SlashCommandBuilder } from "@discordjs/builders";
import { DISCORD_TOKEN } from "./env-vars";
import { commandList } from "./command-info";

export class ExtendedClient extends Client {
  constructor(options: ConstructorParameters<typeof Client>[0]) {
    super(options);
    this.commands = new Collection();
    commandList.forEach((command) =>
      this.commands.set(command.data.name, command)
    );
  }
  public commands: Collection<
    string,
    {
      data: SlashCommandBuilder;
      execute: (interaction: CommandInteraction) => Promise<void>;
    }
  >;
}

const client = new ExtendedClient({ intents: [Intents.FLAGS.GUILDS] });

client.once("ready", () => {
  console.log("Ready!");
});

client.on("interactionCreate", async (interaction) => {
  if (!interaction.isCommand()) return;

  const command = client.commands.get(interaction.commandName);

  if (!command) return;

  try {
    await command.execute(interaction);
  } catch (error) {
    console.error(error);

    await interaction[
      interaction.deferred || interaction.replied ? "editReply" : "reply"
    ]({
      content: "There was an error while executing this command!",
      ephemeral: true,
    });
  }
});

client.login(DISCORD_TOKEN);
