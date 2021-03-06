import createConnectionPool, { sql } from "@databases/pg";
import tableMaker from "@databases/pg-typed";
import { SERVER_DATABASE_URL, USER_DATABASE_URL } from "./env-vars";

import ServerDndSchema, {
  serializeValue as serializeServerDndValue,
} from "./generated_schema/server_data/dnd_5e";
import ServerSchema, {
  serializeValue as serializeServerValue,
} from "./generated_schema/server_data/public";
import ServerPTASchema, {
  serializeValue as serializeServerPTAValue,
} from "./generated_schema/server_data/pta_03";
import ServerPTU05Schema, {
  serializeValue as serializeServerPTU05Value,
} from "./generated_schema/server_data/ptu_05";
import ServerPTUPTSchema, {
  serializeValue as serializeServerPTUPTValue,
} from "./generated_schema/server_data/ptu_pt";
import ServerPTUALSchema, {
  serializeValue as serializeServerPTUALValue,
} from "./generated_schema/server_data/ptu_al";
import ServerPTUGASchema, {
  serializeValue as serializeServerPTUGAValue,
} from "./generated_schema/server_data/ptu_ga";
import ServerPTUHISchema, {
  serializeValue as serializeServerPTUHIValue,
} from "./generated_schema/server_data/ptu_hi";
import ServerZeldaSchema, {
  serializeValue as serializeServerZeldaValue,
} from "./generated_schema/server_data/zelda_rw";
import UserSchema, {
  serializeValue as serializeUserValue,
} from "./generated_schema/user_data";
import Channels from "./generated_schema/user_data/channels";
import Servers from "./generated_schema/user_data/servers";
import { pipe } from "fp-ts/lib/function";

export { sql };

export namespace Server {
  export const tables = tableMaker<ServerSchema>({
    serializeValue: serializeServerValue,
  });
  export const codexTables = {
    dnd_5e: tableMaker<ServerDndSchema>({
      serializeValue: serializeServerDndValue,
    }),
    pta_03: tableMaker<ServerPTASchema>({
      serializeValue: serializeServerPTAValue,
    }),
    ptu_05: tableMaker<ServerPTU05Schema>({
      serializeValue: serializeServerPTU05Value,
    }),
    ptu_pt: tableMaker<ServerPTUPTSchema>({
      serializeValue: serializeServerPTUPTValue,
    }),
    ptu_al: tableMaker<ServerPTUALSchema>({
      serializeValue: serializeServerPTUALValue,
    }),
    ptu_ga: tableMaker<ServerPTUGASchema>({
      serializeValue: serializeServerPTUGAValue,
    }),
    ptu_hi: tableMaker<ServerPTUHISchema>({
      serializeValue: serializeServerPTUHIValue,
    }),
    zelda_rw: tableMaker<ServerZeldaSchema>({
      serializeValue: serializeServerZeldaValue,
    }),
  };
  export const db = createConnectionPool(SERVER_DATABASE_URL);
}

export namespace User {
  export const tables = tableMaker<UserSchema>({
    serializeValue: serializeUserValue,
  });
  export const db = createConnectionPool(USER_DATABASE_URL);
}

process.once("SIGTERM", () => {
  Server.db.dispose().catch((ex) => {
    console.error(ex);
  });
  User.db.dispose().catch((ex) => {
    console.error(ex);
  });
});

export async function fetchSharedEntry(params: {
  target: "channel" | "server";
  server_id: string;
  channel_id?: string;
}): Promise<["channel" | "server", Servers | Channels | null]>;
export async function fetchSharedEntry<
  Item extends keyof Channels & keyof Servers
>(params: {
  target: "channel" | "server";
  server_id: string;
  channel_id?: string;
  item: Item;
}): Promise<
  [
    "channel" | "server",
    Servers | Channels | null,
    Servers[Item] | Channels[Item] | null
  ]
>;
export async function fetchSharedEntry<
  Item extends Exclude<keyof Channels, keyof Servers>
>(params: {
  target: "channel" | "server";
  server_id: string;
  channel_id?: string;
  item: Item;
}): Promise<["channel" | "server", Channels | null, Channels[Item] | null]>;
export async function fetchSharedEntry<
  Item extends Exclude<keyof Servers, keyof Channels>
>(params: {
  target: "channel" | "server";
  server_id: string;
  channel_id?: string;
  item: Item;
}): Promise<["channel" | "server", Servers | null, Servers[Item] | null]>;
export async function fetchSharedEntry({
  target,
  server_id,
  channel_id,
  item,
}: {
  target: "channel" | "server";
  server_id: string;
  channel_id?: string;
  item?: keyof Channels | keyof Servers;
}): Promise<
  | ["channel" | "server", Servers | Channels | null]
  | [
      "channel" | "server",
      Servers | Channels | null,
      Servers[keyof Servers] | Channels[keyof Channels] | null
    ]
> {
  const channelEntries =
    target === "channel" && channel_id
      ? await User.tables.channels(User.db).findOne({ server_id, channel_id })
      : undefined;
  const channelValue =
    channelEntries && item ? channelEntries[item] : channelEntries;
  return [
    channelEntries ? "channel" : "server",
    ...(channelValue
      ? item
        ? [channelEntries ?? null, channelValue ?? null]
        : [channelEntries ?? null]
      : pipe(
          await User.tables.servers(User.db).findOne({ server_id }),
          item
            ? (e) =>
                e && item in e ? [e, e[item as keyof Servers]] : [null, null]
            : (e) => [e]
        )),
    // resolve this later
  ] as any;
}

export async function fetchChannelEntry(
  server_id: string,
  channel_id?: string
): Promise<Servers | Channels | null> {
  const channelEntries =
    channel_id &&
    (await User.tables.channels(User.db).findOne({ server_id, channel_id }));
  return (
    channelEntries ||
    (await User.tables.servers(User.db).findOne({ server_id }))
  );
}
