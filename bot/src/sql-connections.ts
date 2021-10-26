import createConnectionPool, { sql } from "@databases/pg";
import tableMaker from "@databases/pg-typed";
import { SERVER_DATABASE_URL, USER_DATABASE_URL } from "./env-vars";

import ServerSchema, {
  serializeValue as serializeServerValue,
} from "./generated_schema/server_data";
import UserSchema, {
  serializeValue as serializeUserValue,
} from "./generated_schema/user_data";
import Channels from "./generated_schema/user_data/channels";
import Servers from "./generated_schema/user_data/servers";

export { sql };

export namespace Server {
  export const tables = tableMaker<ServerSchema>({
    serializeValue: serializeServerValue,
  });
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

export async function fetchSharedEntry(
  target: "channel" | "server",
  server_id: string,
  channel_id?: string
): Promise<["channel" | "server", Servers | Channels | null]> {
  const channelEntries =
    target === "channel" &&
    channel_id &&
    (await User.tables.channels(User.db).findOne({ server_id, channel_id }));
  return [
    channelEntries ? "channel" : "server",
    channelEntries ||
      (await User.tables.servers(User.db).findOne({ server_id })),
  ];
}
