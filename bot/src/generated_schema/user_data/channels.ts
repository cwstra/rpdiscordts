/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: qYvQBZufwM39p8F8UkpBkk+r5nik6J1HWnehO9zRqzw12lMXWFjKGdrO+QQdxcCY2TriMRbYLNTi27aqnCh6Fw==
 */

/* eslint-disable */
// tslint:disable

interface Channels {
  channel_id: string & {readonly __brand?: 'channels_channel_id'}
  charseps: (Array<string | null>) | null
  charsigns: (Array<string | null>) | null
  codex: (number) | null
  ephemeral: (boolean) | null
  freeze_on_timeout: (boolean) | null
  inline: (boolean) | null
  server_id: string & {readonly __brand?: 'channels_server_id'}
}
export default Channels;

interface Channels_InsertParameters {
  channel_id: string & {readonly __brand?: 'channels_channel_id'}
  charseps?: (Array<string | null>) | null
  charsigns?: (Array<string | null>) | null
  codex?: (number) | null
  ephemeral?: (boolean) | null
  freeze_on_timeout?: (boolean) | null
  inline?: (boolean) | null
  server_id: string & {readonly __brand?: 'channels_server_id'}
}
export type {Channels_InsertParameters}
