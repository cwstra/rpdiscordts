import { REST } from "@discordjs/rest";
import { Routes } from "discord-api-types/v9";
import { CLIENT_ID, SERVER_ID, DISCORD_TOKEN } from "./env-vars";
import * as yargs from "yargs";
import { assert } from "./helpers/general";
import { commandList } from "./command-info";
import { Client, GatewayIntentBits } from "discord.js";

(async () => {
  const {
    _: [deployType],
  } = await yargs
    .command(
      "deploy-commands [deployType]",
      "deploy commands to the server and client listed in .env, or, if deployType is 'global', globally.",
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

  const commandData = commandList.map((command) => command.data.toJSON());

  const rest = new REST({ version: "9" }).setToken(DISCORD_TOKEN);

  console.log(commandData);

  await rest.put(route, {
    body: commandData,
  });

  console.log("Successfully registered application commands.");

  const commandsWithPermissions = commandList.filter(
    (c): c is typeof c & Required<Pick<typeof c, "permissions">> =>
      !!c.permissions?.length
  );

  if (!commandsWithPermissions.length) {
    console.log("No commands with special permissions. Done.");
    return;
  }

  const commandNameIds = (await rest.get(route)) as {
    id: string;
    name: string;
  }[];

  const client = new Client({ intents: [ GatewayIntentBits.Guilds] });
  client.login(DISCORD_TOKEN);
  if (!client.application?.owner) await client.application?.fetch();

  const commandManager =
    deployType === "global"
      ? client.application?.commands
      : client.guilds.cache.get(SERVER_ID ?? "")?.commands;
  if (!commandManager) {
    console.log("Failed to initialize manager");
    return;
  }
  await Promise.all(
    commandsWithPermissions.map(async ({ data, permissions }) => {
      const id = commandNameIds.find(({ name }) => name === data.name)?.id;
      if (!id) return;
      const command = await commandManager.fetch(id);
      await command.permissions.set({ token: DISCORD_TOKEN, permissions });
    })
  );
})();
