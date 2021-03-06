/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: e7G+Jk+tg5OKPZVkiIsgGPsDwN8bq7oIs560sDnna1NagDqsK+v+ixGLpTjWV6VzzQPDlEiHP5qISnpWu53Pgw==
 */

/* eslint-disable */
// tslint:disable

interface Characters {
  /**
   * @default nextval('characters_character_id_seq'::regclass)
   */
  character_id: number & {readonly __brand?: 'characters_character_id'}
  character_name: string
  member_id: string
  server_id: string
}
export default Characters;

interface Characters_InsertParameters {
  /**
   * @default nextval('characters_character_id_seq'::regclass)
   */
  character_id?: number & {readonly __brand?: 'characters_character_id'}
  character_name: string
  member_id: string
  server_id: string
}
export type {Characters_InsertParameters}
