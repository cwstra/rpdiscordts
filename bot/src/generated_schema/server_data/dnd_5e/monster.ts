/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: +uIaCNqNz2L7nC9zjOg7JKpkuOPyrxMHOL9nGdRo90sAJA5NVUWzfL8lqOoXcLIpLoaEItOQ2WFUzEQLJ0aFgA==
 */

/* eslint-disable */
// tslint:disable

interface Monster {
  embed: any
  id: string & {readonly __brand?: 'monster_id'}
}
export default Monster;

interface Monster_InsertParameters {
  embed: any
  id: string & {readonly __brand?: 'monster_id'}
}
export type {Monster_InsertParameters}
