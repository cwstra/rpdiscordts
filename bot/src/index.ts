import {
  AutocompleteInteraction,
  Client,
  Collection,
  CommandInteraction,
  Intents,
} from "discord.js";
import { SlashCommandBuilder } from "@discordjs/builders";
import { DISCORD_TOKEN } from "./env-vars";
import { commandList } from "./command-info";
import { User } from "./sql-connections";
import { sql } from "@databases/pg";

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
      autoComplete?: (interaction: AutocompleteInteraction) => Promise<void>;
    }
  >;
}

const client = new ExtendedClient({ intents: [Intents.FLAGS.GUILDS] });

client.once("ready", () => {
  console.log("Ready!");
});

client.on("interactionCreate", async (interaction) => {
  if (interaction.isCommand()) {
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

    try {
      await User.db.query(sql`
      insert into command_usage(name, uses)
      values (${interaction.commandName}, 1)
      on conflict (name)
      do update set uses = command_usage.uses + 1
    `);
      await User.tables
        .unique_users(User.db)
        .insertOrIgnore({ user_id: interaction.user.id });
      if (interaction.guildId)
        await User.tables
          .unique_servers(User.db)
          .insertOrIgnore({ server_id: interaction.guildId });
    } catch (error) {
      console.error("Command error: ", error);
    }
  } else if (interaction.isAutocomplete()) {
    const command = client.commands.get(interaction.commandName);

    if (!command?.autoComplete) return;

    try {
      await command.autoComplete(interaction);
    } catch (error) {
      console.error("Autocomplete Error:", error);
    }
  }
});

client.login(DISCORD_TOKEN);
