import { Channel, CommandInteraction, Role, User } from "discord.js";
import { SlashCommandBuilder } from "@discordjs/builders";

/*
type Named = { name: string; description: string };
type PickNames<
  NamedValues extends Named,
  Names extends NamedValues["name"]
> = Extract<NamedValues, { name: Names }>;
*/

namespace Option {
  type Base = { description: string; required: boolean };
  export type String = Base & {
    type: "string";
    choices?: [name: string, value: string][];
  };
  export type Integer = Base & {
    type: "integer";
    choices?: [name: string, value: number][];
  };
  export type Number = Base & {
    type: "number";
    choices?: [name: string, value: number][];
  };
  export type Boolean = Base & {
    type: "boolean";
  };
  export type User = Base & {
    type: "user";
  };
  export type Channel = Base & {
    type: "channel";
  };
  export type Role = Base & {
    type: "role";
  };
  export type Mentionable = Base & {
    type: "mentionable";
  };
}

type Option =
  | Option.String
  | Option.Integer
  | Option.Number
  | Option.Boolean
  | Option.User
  | Option.Channel
  | Option.Role
  | Option.Mentionable;

type OptionToType<O extends Option> =
  | (O extends Option.String
      ? O extends { choices: [string, infer Value][] }
        ? Value
        : string
      : O extends Option.Integer
      ? O extends { choices: [string, infer Value][] }
        ? Value
        : number
      : O extends Option.Number
      ? O extends { choices: [string, infer Value][] }
        ? Value
        : number
      : O extends Option.Boolean
      ? boolean
      : O extends Option.User
      ? User
      : O extends Option.Channel
      ? Channel
      : O extends Option.Role
      ? Role
      : O extends Option.Mentionable
      ? NonNullable<ReturnType<CommandInteraction["options"]["getMentionable"]>>
      : never)
  | (O["required"] extends true ? never : null);

type OptionMap = Record<string, Option>;

type OptionResultMap<M extends OptionMap> = {
  [N in keyof M]: M[N] extends infer O
    ? O extends Option
      ?
          | (O extends Option.String
              ? O extends { choices: [string, infer Value][] }
                ? Value
                : string
              : O extends Option.Integer
              ? O extends { choices: [string, infer Value][] }
                ? Value
                : number
              : O extends Option.Number
              ? O extends { choices: [string, infer Value][] }
                ? Value
                : number
              : O extends Option.Boolean
              ? boolean
              : O extends Option.User
              ? User
              : O extends Option.Channel
              ? Channel
              : O extends Option.Role
              ? Role
              : O extends Option.Mentionable
              ? NonNullable<
                  ReturnType<CommandInteraction["options"]["getMentionable"]>
                >
              : never)
          | (O["required"] extends true ? never : null)
      : never
    : never;
};

const optionMapToResultMap = (i: CommandInteraction, options: OptionMap) =>
  Object.fromEntries(
    Object.entries(options).map(([name, value]) => [
      name,
      (() => {
        switch (value.type) {
          case "string":
            return i.options.getString(name, value.required);
          case "integer":
            return i.options.getInteger(name, value.required);
          case "number":
            return i.options.getNumber(name, value.required);
          case "boolean":
            return i.options.getBoolean(name, value.required);
          case "user":
            return i.options.getUser(name, value.required);
          case "channel":
            return i.options.getChannel(name, value.required);
          case "role":
            return i.options.getRole(name, value.required);
          case "mentionable":
            return i.options.getMentionable(name, value.required);
          default: {
            const e: never = value;
            return e;
          }
        }
      })(),
    ])
  );

type SubCommandArgs<M extends OptionMap> = {
  description: string;
  options?: M;
};

type SubCommandMap = Record<string, SubCommandArgs<OptionMap>>;

type SubCommandResult<
  SCMap extends SubCommandMap,
  Name extends keyof SCMap
> = SCMap[Name] extends SubCommandArgs<infer OM>
  ? SCMap[Name] extends { options: any[] }
    ? { subCommand: Name; options: OM }
    : { subCommand: Name }
  : never;

type SubCommandGroupArgs<M extends SubCommandMap> = {
  description: string;
  subCommands: M;
};

type SubCommandGroupMap = Record<string, SubCommandGroupArgs<SubCommandMap>>;

type SubCommandGroupResult<
  SCGMap extends SubCommandGroupMap,
  Name extends keyof SCGMap
> = SCGMap[Name] extends SubCommandGroupArgs<infer M>
  ? { group: Name } & SubCommandResult<M, keyof M>
  : never;

type BasicCommand<M extends OptionMap> = {
  name: string;
  description: string;
  options?: M;
  execute: (
    interaction: CommandInteraction,
    options: OptionResultMap<M>
  ) => Promise<void>;
};

type FlatCommand<M extends SubCommandMap> = {
  name: string;
  description: string;
  subcommands: M;
  execute: (
    interaction: CommandInteraction,
    options: SubCommandResult<M, keyof M>
  ) => Promise<void>;
};

type GroupCommand<
  SubCommandGroups extends SubCommandGroupMap,
  SubCommands extends SubCommandMap
> = {
  name: string;
  description: string;
  subcommandGroups: SubCommandGroups;
  subcommands?: SubCommands;
  execute: (
    interaction: CommandInteraction,
    options:
      | SubCommandGroupResult<SubCommandGroups, keyof SubCommandGroups>
      | SubCommandResult<SubCommands, keyof SubCommands>
  ) => Promise<void>;
};

function addOption(
  builder:
    | SlashCommandBuilder
    | Exclude<
        Parameters<SlashCommandBuilder["addSubcommand"]>[0],
        (...args: any[]) => any
      >,
  name: string,
  option: Option
): void {
  switch (option.type) {
    case "string":
      builder.addStringOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        if (option.choices) o.addChoices(option.choices);
        return o;
      });
      return;
    case "integer":
      builder.addIntegerOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        if (option.choices) o.addChoices(option.choices);
        return o;
      });
      return;
    case "number":
      builder.addNumberOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        if (option.choices) o.addChoices(option.choices);
        return o;
      });
      return;
    case "boolean":
      builder.addBooleanOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        return o;
      });
      return;
    case "user":
      builder.addUserOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        return o;
      });
      return;
    case "channel":
      builder.addChannelOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        return o;
      });
      return;
    case "role":
      builder.addRoleOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        return o;
      });
      return;
    case "mentionable":
      builder.addMentionableOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        return o;
      });
      return;
    default: {
      const e: never = option;
      return e;
    }
  }
}

function addSubCommand(
  builder:
    | SlashCommandBuilder
    | Exclude<
        Parameters<SlashCommandBuilder["addSubcommandGroup"]>[0],
        (...args: any[]) => any
      >,
  name: string,
  args: SubCommandArgs<OptionMap>
): void {
  builder.addSubcommand((newSubCommand) => {
    newSubCommand.setName(name).setDescription(args.description);
    Object.entries(args.options ?? {}).forEach(([n, o]) =>
      addOption(newSubCommand, n, o)
    );
    return newSubCommand;
  });
}

function addSubCommandGroup(
  builder: SlashCommandBuilder,
  name: string,
  args: SubCommandGroupArgs<SubCommandMap>
): void {
  builder.addSubcommandGroup((newGroup) => {
    newGroup.setName(name).setDescription(args.description);
    Object.entries(args.subCommands).forEach(([n, subCommandData]) => {
      addSubCommand(newGroup, n, subCommandData);
    });
    return newGroup;
  });
}

export function makeCommand<
  GroupMap extends SubCommandGroupMap,
  CommandMap extends SubCommandMap
>(
  args: GroupCommand<GroupMap, CommandMap>
): {
  data: SlashCommandBuilder;
  execute: (interaction: CommandInteraction) => Promise<void>;
};
export function makeCommand<CommandMap extends SubCommandMap>(
  args: FlatCommand<CommandMap>
): {
  data: SlashCommandBuilder;
  execute: (interaction: CommandInteraction) => Promise<void>;
};
export function makeCommand<OMap extends OptionMap>(
  args: BasicCommand<OMap>
): {
  data: SlashCommandBuilder;
  execute: (interaction: CommandInteraction) => Promise<void>;
};
export function makeCommand(
  args:
    | GroupCommand<SubCommandGroupMap, SubCommandMap>
    | FlatCommand<SubCommandMap>
    | BasicCommand<OptionMap>
) {
  const builder = new SlashCommandBuilder()
    .setName(args.name)
    .setDescription(args.description);
  if ("subcommandGroups" in args) {
    Object.entries(args.subcommandGroups).forEach(([n, groupData]) =>
      addSubCommandGroup(builder, n, groupData)
    );
    Object.entries(args.subcommands ?? {}).forEach(([n, subCommandData]) =>
      addSubCommand(builder, n, subCommandData)
    );
    const parseOptions = (i: CommandInteraction) => {
      const group = i.options.getSubcommandGroup() ?? undefined;
      const subCommand = i.options.getSubcommand();
      const options =
        group in args.subcommandGroups
          ? args.subcommandGroups[group].subCommands[subCommand].options
          : args.subcommands?.[subCommand].options;
      return {
        group,
        subCommand,
        options: options ? optionMapToResultMap(i, options) : undefined,
      } as
        | SubCommandGroupResult<SubCommandGroupMap, string>
        | SubCommandResult<SubCommandMap, string>;
    };
    return {
      data: builder,
      execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
    };
  } else if ("subcommands" in args) {
    Object.entries(args.subcommands ?? {}).forEach(([n, subCommandData]) =>
      addSubCommand(builder, n, subCommandData)
    );
    const parseOptions = (i: CommandInteraction) => {
      const subCommand = i.options.getSubcommand();
      const { options } =
        args.subcommands[subCommand as keyof typeof args.subcommands];
      return {
        subCommand,
        options: options ? optionMapToResultMap(i, options) : undefined,
      };
    };
    return {
      data: builder,
      execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
    };
  } else {
    Object.entries(args.options ?? {}).forEach(([n, o]) =>
      addOption(builder, n, o)
    );
    const parseOptions = args.options
      ? (i: CommandInteraction) =>
          optionMapToResultMap(i, args.options!) as OptionResultMap<OptionMap>
      : () => ({});
    return {
      data: builder,
      execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
    };
  }
}
