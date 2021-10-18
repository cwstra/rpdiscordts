import { config } from "dotenv";
import { assert } from "./helpers";

config();

const {
  CLIENT_ID: maybeClient,
  SERVER_ID,
  DISCORD_TOKEN: maybeToken,

  BOT_GIT_URL,
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,

  DONATION_TEXT,
} = process.env;

assert(maybeClient, "No client id set");
assert(maybeToken, "No discord token set");

const client = maybeClient!;
const token = maybeToken!;

export {
  client as CLIENT_ID,
  SERVER_ID,
  token as DISCORD_TOKEN,
  BOT_GIT_URL,
  BOT_DISCORD_SERVER,
  BOT_DOC_URL,
  DONATION_TEXT,
};
