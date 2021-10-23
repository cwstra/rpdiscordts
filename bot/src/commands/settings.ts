import {
  makeCommand,
  makeSubCommands,
  SubCommandResult,
} from "../make-command";

async function symbolListChange() {
  console.log("");
}

const settingGroup = (target: "server" | "channel") => ({
  description: `Check or change ${target} settings`,
  subCommands: makeSubCommands({
    charsign: {
      description: `Check or change the current ${target} charsign for roll queries`,
      options: {
        charsign: {
          type: "string",
          description: `If given, set the ${target}'s charsign to this symbol. Otherwise, get the ${target}'s current charsign.`,
          required: false,
        },
      },
    },
    charsep: {
      description: `Check or change the current ${target}'s charsep for roll queries`,
      options: {
        charsep: {
          type: "string",
          description: `If given, set the ${target}'s charsep to this symbol. Otherwise, get the ${target}'s current charsep.`,
          required: false,
        },
      },
    },
    codex: {
      description: `Check or change the current ${target}'s lookup codex.`,
    },
    onClose: {
      description: `Toggle paginator freezing in the current ${target}.`,
    },
    inlineToggle: {
      description: `Check or change the closing behavior of reference embeds in the current ${target}.`,
    },
  }),
});

module.exports = makeCommand({
  name: "settings",
  description: "Check or change bot settings",
  subcommandGroups: {
    server: settingGroup("server"),
    channel: settingGroup("channel"),
  },
  subcommands: {
    botModRoles: {
      description:
        "Toggles whether the provided role can alter the bot's settings on this server. Users with admin privileges will always be able to change the settings.",
      options: {
        role: {
          type: "role",
          description:
            "If given, prompt to toggle this role's ability to change bot settings. Otherwise, check what roles can edit the bot settings.",
          required: false,
        },
      },
    },
  },
  async execute(interaction, options) {
    type Tester = Extract<
      typeof options,
      { subCommand: "charsign" | "charsep" }
    >;
  },
});
