import {
  ApplicationCommandOptionChoice,
  ApplicationCommandPermissionData,
  AutocompleteInteraction,
  Channel,
  CommandInteraction,
  Role,
  User,
} from "discord.js";
import { SlashCommandBuilder } from "@discordjs/builders";
import { flattenedUnion } from "./helpers/general";
import { Simplify } from "./helpers/types";

namespace Option {
  type Base = { description: string; required: boolean };
  export type String = Base & {
    type: "string";
    choices?: [name: string, value: string][];
    autoComplete?: boolean;
  };
  export type Integer = Base & {
    type: "integer";
    choices?: [name: string, value: number][];
    autoComplete?: boolean;
  };
  export type Number = Base & {
    type: "number";
    choices?: [name: string, value: number][];
    autoComplete?: boolean;
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

type OptionMap = Record<string, Option>;

export function makeOptions<Mapping extends Record<string, Option>>(
  mapping: Mapping
) {
  return mapping;
}

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

type AutoOptionResultMap<M extends OptionMap> = {
  [N in keyof M]: M[N] extends infer O
    ? O extends { autoComplete?: infer T }
      ? true extends T
        ? N
        : never
      : never
    : never;
}[keyof M] extends infer Focused
  ? Focused extends never
    ? never
    : {
        options: {
          [N in keyof M]: M[N] extends infer O
            ? O extends Option
              ?
                  | (O extends Option.String
                      ?
                          | (O extends { choices: [string, infer Value][] }
                              ? Value
                              : string)
                          | undefined
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
                          ReturnType<
                            CommandInteraction["options"]["getMentionable"]
                          >
                        >
                      : never)
                  | null
              : never
            : never;
        };
        focusedOption: Focused;
      }
  : never;

const optionMapToResultMap = (
  i: CommandInteraction | AutocompleteInteraction,
  options: OptionMap,
  forAuto?: ApplicationCommandOptionChoice
) =>
  Object.fromEntries(
    Object.entries(options).map(([name, value]) => [
      name,
      (() => {
        const req = forAuto ? false : value.required;
        if (name === forAuto?.name) return forAuto?.value;
        switch (value.type) {
          case "string":
            return i.options.getString(name, req);
          case "integer":
            return i.options.getInteger(name, req);
          case "number":
            return i.options.getNumber(name, req);
          case "boolean":
            return i.options.getBoolean(name, req);
          case "user":
            return i.options.getUser(name, req);
          case "channel":
            return i.options.getChannel(name, req);
          case "role":
            return i.options.getRole(name, req);
          case "mentionable":
            return i.options.getMentionable(name, req);
          default: {
            const e: never = value;
            return e;
          }
        }
      })(),
    ])
  );
type EmptySubCommand = { description: string };
type OptionSubCommand<M extends OptionMap> = {
  description: string;
  options: M;
};

type SubCommandMap = Record<
  string,
  OptionSubCommand<OptionMap> | EmptySubCommand
>;

export function makeSubCommands<Mapping extends SubCommandMap>(
  mapping: Mapping
) {
  return mapping;
}

export type SubCommandResult<
  SCMap extends SubCommandMap,
  Name
> = Name extends keyof SCMap
  ? SCMap[Name] extends OptionSubCommand<infer O>
    ? { subCommand: Name; options: Simplify<OptionResultMap<O>> }
    : { subCommand: Name }
  : never;

export type SubAutoCommandResult<
  SCMap extends SubCommandMap,
  Name
> = Name extends keyof SCMap
  ? SCMap[Name] extends OptionSubCommand<infer O>
    ? { subCommand: Name } & Simplify<AutoOptionResultMap<O>>
    : {}
  : never;

type SubCommandGroupArgs<M extends SubCommandMap> = {
  description: string;
  subCommands: M;
};

type SubCommandGroupMap = Record<string, SubCommandGroupArgs<SubCommandMap>>;

export function makeSubCommandGroups<
  Mapping extends Record<string, SubCommandGroupArgs<SubCommandMap>>
>(mapping: Mapping) {
  return mapping;
}

type SubCommandGroupResult<
  SCGMap extends SubCommandGroupMap,
  Name
> = Name extends keyof SCGMap
  ? SCGMap[Name] extends SubCommandGroupArgs<infer SCMap>
    ? { group: Name } & SubCommandResult<SCMap, keyof SCMap>
    : never
  : never;

type SubAutoCommandGroupResult<
  SCGMap extends SubCommandGroupMap,
  Name
> = Name extends keyof SCGMap
  ? SCGMap[Name] extends SubCommandGroupArgs<infer SCMap>
    ? { group: Name } & SubAutoCommandResult<SCMap, keyof SCMap>
    : never
  : never;

type BaseCommand<Opts, AutoOpts = never> = {
  name: string;
  description: string;
  defaultPermission?: boolean;
  permissions?: ApplicationCommandPermissionData[];
  execute: (interaction: CommandInteraction, options: Opts) => Promise<void>;
  autoComplete?: (
    interaction: AutocompleteInteraction,
    options: AutoOpts
  ) => Promise<void>;
};

type BasicCommand<M extends OptionMap> = BaseCommand<
  OptionResultMap<M>,
  AutoOptionResultMap<M>
> & {
  options?: M;
};

export type FlatCommand<M extends SubCommandMap> = BaseCommand<
  SubCommandResult<M, keyof M>,
  SubAutoCommandResult<M, keyof M>
> & {
  subcommands: M;
};

type GroupCommand<SubCommandGroups extends SubCommandGroupMap> = BaseCommand<
  SubCommandGroupResult<SubCommandGroups, keyof SubCommandGroups>,
  SubAutoCommandGroupResult<SubCommandGroups, keyof SubCommandGroups>
> & {
  subcommandGroups: SubCommandGroups;
};

type JointCommand<
  SubCommandGroups extends SubCommandGroupMap,
  SubCommands extends SubCommandMap
> = BaseCommand<
  | SubCommandGroupResult<SubCommandGroups, keyof SubCommandGroups>
  | SubCommandResult<SubCommands, keyof SubCommands>,
  | SubAutoCommandGroupResult<SubCommandGroups, keyof SubCommandGroups>
  | SubAutoCommandResult<SubCommands, keyof SubCommands>
> & {
  subcommandGroups: SubCommandGroups;
  subcommands: SubCommands;
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
        if (option.autoComplete) o.setAutocomplete(true);
        return o;
      });
      return;
    case "integer":
      builder.addIntegerOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        if (option.choices) o.addChoices(option.choices);
        if (option.autoComplete) o.setAutocomplete(true);
        return o;
      });
      return;
    case "number":
      builder.addNumberOption((o) => {
        o.setName(name).setDescription(option.description);
        if (option.required) o.setRequired(true);
        if (option.choices) o.addChoices(option.choices);
        if (option.autoComplete) o.setAutocomplete(true);
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
  args: OptionSubCommand<OptionMap> | EmptySubCommand
): void {
  builder.addSubcommand((newSubCommand) => {
    newSubCommand.setName(name).setDescription(args.description);
    if ("options" in args)
      Object.entries(args.options).forEach(([n, o]) =>
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

export type CommandDef = {
  data: SlashCommandBuilder;
  permissions?: ApplicationCommandPermissionData[];
  execute: (interaction: CommandInteraction) => Promise<void>;
  autoComplete?: (interaction: AutocompleteInteraction) => Promise<void>;
};

export function makeCommand<
  GroupMap extends SubCommandGroupMap,
  CommandMap extends SubCommandMap
>(args: JointCommand<GroupMap, CommandMap>): CommandDef;
export function makeCommand<GroupMap extends SubCommandGroupMap>(
  args: GroupCommand<GroupMap>
): CommandDef;
export function makeCommand<CommandMap extends SubCommandMap>(
  args: FlatCommand<CommandMap>
): CommandDef;
export function makeCommand<OMap extends OptionMap>(
  args: BasicCommand<OMap>
): CommandDef;
export function makeCommand(
  args:
    | JointCommand<SubCommandGroupMap, SubCommandMap>
    | GroupCommand<SubCommandGroupMap>
    | FlatCommand<SubCommandMap>
    | BasicCommand<OptionMap>
) {
  const builder = new SlashCommandBuilder()
    .setName(args.name)
    .setDescription(args.description);
  if (args.defaultPermission !== undefined)
    builder.setDefaultPermission(args.defaultPermission);
  if ("subcommandGroups" in args) {
    Object.entries(args.subcommandGroups).forEach(([n, groupData]) =>
      addSubCommandGroup(builder, n, groupData)
    );
    if ("subcommands" in args) {
      Object.entries(args.subcommands).forEach(([n, subCommandData]) =>
        addSubCommand(builder, n, subCommandData)
      );

      const parseOptions = (
        i: CommandInteraction | AutocompleteInteraction
      ) => {
        const subCommand = i.options.getSubcommand();
        const group =
          subCommand in args.subcommands
            ? undefined
            : i.options.getSubcommandGroup() ?? undefined;
        const { options } = !group
          ? flattenedUnion(args.subcommands[subCommand])
          : flattenedUnion(
              args.subcommandGroups[group].subCommands[subCommand]
            );
        const focused = i.isAutocomplete()
          ? i.options.getFocused(true)
          : undefined;
        return {
          group,
          subCommand,
          options: options
            ? optionMapToResultMap(i, options, focused)
            : undefined,
          focusedOption: focused?.name,
        } as any;
      };
      return {
        data: builder,
        permissions: args.permissions,
        execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
        autoComplete:
          args.autoComplete &&
          (async (i: AutocompleteInteraction) =>
            await args.autoComplete?.(i, parseOptions(i))),
      };
    } else {
      const parseOptions = (
        i: CommandInteraction | AutocompleteInteraction
      ) => {
        const group = i.options.getSubcommandGroup() ?? undefined;
        const subCommand = i.options.getSubcommand();
        const { options } = flattenedUnion(
          args.subcommandGroups[group].subCommands[subCommand]
        );
        const focused = i.isAutocomplete()
          ? i.options.getFocused(true)
          : undefined;
        return {
          group,
          subCommand,
          options: options
            ? optionMapToResultMap(i, options, focused)
            : undefined,
          focusedOption: focused?.name,
        } as any;
      };
      return {
        data: builder,
        permissions: args.permissions,
        execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
        autoComplete:
          args.autoComplete &&
          (async (i: AutocompleteInteraction) =>
            await args.autoComplete?.(i, parseOptions(i))),
      };
    }
  } else if ("subcommands" in args) {
    Object.entries(args.subcommands ?? {}).forEach(([n, subCommandData]) =>
      addSubCommand(builder, n, subCommandData)
    );
    const parseOptions = (i: CommandInteraction | AutocompleteInteraction) => {
      const subCommandName = i.options.getSubcommand();
      const subCommand =
        args.subcommands[subCommandName as keyof typeof args.subcommands];
      const { options } = flattenedUnion(subCommand);
      const focused = i.isAutocomplete()
        ? i.options.getFocused(true)
        : undefined;
      return {
        subCommand: subCommandName,
        options: options
          ? optionMapToResultMap(i, options, focused)
          : undefined,
        focusedOption: focused?.name,
      } as any;
    };
    return {
      data: builder,
      permissions: args.permissions,
      execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
      autoComplete:
        args.autoComplete &&
        (async (i: AutocompleteInteraction) =>
          await args.autoComplete?.(i, parseOptions(i))),
    };
  } else {
    Object.entries(args.options ?? {}).forEach(([n, o]) =>
      addOption(builder, n, o)
    );
    const parseOptions = args.options
      ? (i: CommandInteraction | AutocompleteInteraction) => {
          const focused = i.isAutocomplete()
            ? i.options.getFocused(true)
            : undefined;
          return {
            options: optionMapToResultMap(i, args.options!, focused),
            focusedOption: focused?.name,
          } as any;
        }
      : () => ({});
    return {
      data: builder,
      permissions: args.permissions,
      execute: (i: CommandInteraction) => args.execute(i, parseOptions(i)),
      autoComplete:
        args.autoComplete &&
        (async (i: AutocompleteInteraction) =>
          await args.autoComplete?.(i, parseOptions(i))),
    };
  }
}
