import { config } from "dotenv";
import { assert } from "./helpers/general";

config();

const {
  CLIENT_ID: maybeClient,
  SERVER_ID,
  DISCORD_TOKEN: maybeToken,

  BOT_GIT_URL,
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,

  DONATION_TEXT,

  SERVER_DATABASE_URL: maybeServerUrl,
  USER_DATABASE_URL: maybeUserUrl,

  ROLL_URL: maybeRollUrl,
  ROLL_PORT: maybeRollPort,
  ROLL_PATH: maybeRollPath,
} = process.env;

assert(maybeClient, "No client id set");
assert(maybeToken, "No discord token set");
assert(maybeServerUrl, "No server url set");
assert(maybeUserUrl, "No user url set");
assert(maybeRollUrl, "No roll url set");
assert(maybeRollPort, "No roll url set");
assert(maybeRollPath, "No roll path set");

const CLIENT_ID = maybeClient!;
const DISCORD_TOKEN = maybeToken!;
const SERVER_DATABASE_URL = maybeServerUrl!;
const USER_DATABASE_URL = maybeUserUrl!;
const ROLL_URL = maybeRollUrl!;
const ROLL_PORT = maybeRollPort!;
const ROLL_PATH = maybeRollPath!;

export {
  CLIENT_ID,
  SERVER_ID,
  DISCORD_TOKEN,
  BOT_GIT_URL,
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,
  DONATION_TEXT,
  SERVER_DATABASE_URL,
  USER_DATABASE_URL,
  ROLL_URL,
  ROLL_PORT,
  ROLL_PATH,
};
