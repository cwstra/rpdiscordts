import { join } from "path";
import * as fs from "fs";
import { CommandDef } from "./make-command";

const commandDir = join(__dirname, "commands");

export const commandList = fs
  .readdirSync(commandDir)
  .filter((file) => file.endsWith(".js") && !file.endsWith(".wip.js"))
  .flatMap((file): CommandDef[] => {
    console.log(`Loading ${join("commands", file)}`);
    const commandOrCommands = require(join(commandDir, file));
    return "commands" in commandOrCommands
      ? commandOrCommands["commands"]
      : [commandOrCommands];
  });
