/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: dFWJ2payqNvOXINQsYzkoaSICsWXmj6BkL6Au2hWB8CKnJ6r4Mr4G6AEgppMfinLs98IF4zkHIOD4IZjwIT8MQ==
 */

/* eslint-disable */
// tslint:disable

interface MoveData {
  book: string
  category: string
  contest_kw: (string) | null
  contest_stat: (string) | null
  damage: (string) | null
  effect: (string) | null
  ele_type: string
  frequency: (string) | null
  /**
   * @default nextval('pta_03.move_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'move_data_id'}
  legendary: boolean
  name: string
  page_num: number
  range: string
  skills: (Array<string | null>) | null
}
export default MoveData;

interface MoveData_InsertParameters {
  book: string
  category: string
  contest_kw?: (string) | null
  contest_stat?: (string) | null
  damage?: (string) | null
  effect?: (string) | null
  ele_type: string
  frequency?: (string) | null
  /**
   * @default nextval('pta_03.move_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'move_data_id'}
  legendary: boolean
  name: string
  page_num: number
  range: string
  skills?: (Array<string | null>) | null
}
export type {MoveData_InsertParameters}
