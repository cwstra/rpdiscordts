import { REST } from "@discordjs/rest";
import { Routes } from "discord-api-types/v9";
import { CLIENT_ID, SERVER_ID, DISCORD_TOKEN } from "./env-vars";
import * as yargs from "yargs";
import { assert } from "./helpers";
import { commandList } from "./command-helpers";

(async () => {
  const {
    _: [deployType],
  } = await yargs
    .command(
      "deploy-commands [deployType]",
      "deploy commands to the server server and client listed in .env, or, if deployType is 'global', globally.",
      (yargs) =>
        yargs.positional("deployType", {
          type: "string",
          describe:
            "If 'global', deploy commands globally. Otherwise, deploy to the testing server + client",
        })
    )
    .help()
    .alias("help", "h").argv;

  const route =
    deployType === "global"
      ? Routes.applicationCommands(CLIENT_ID)
      : SERVER_ID
      ? Routes.applicationGuildCommands(CLIENT_ID, SERVER_ID)
      : undefined;

  assert(route, "No server id for guild command deployment");

  const commands = commandList.map((command) => command.data.toJSON());

  const rest = new REST({ version: "9" }).setToken(DISCORD_TOKEN);

  rest
    .put(route, {
      body: commands,
    })
    .then(() => console.log("Successfully registered application commands."))
    .catch(console.error);
})();
