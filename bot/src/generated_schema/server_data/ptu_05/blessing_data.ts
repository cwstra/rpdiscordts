/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: Ov63S+652ffz5Zlioow5fJDmmPWUvGQO8ZNZOaFIprkAtUn77rUFk9dFbdKsubuPONoRxkilwL2+ToifVzOgfQ==
 */

/* eslint-disable */
// tslint:disable

interface BlessingData {
  book: string
  /**
   * @default nextval('ptu_05.blessing_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'blessing_data_id'}
  messiah_effect: string
  messiah_frequency: string
  messiah_trigger: (string) | null
  name: string
  page_num: number
  prerequisites: (string) | null
  rank: number
  signer_effect: string
  signer_frequency: string
  signer_trigger: (string) | null
  target: (string) | null
  trigger: (string) | null
}
export default BlessingData;

interface BlessingData_InsertParameters {
  book: string
  /**
   * @default nextval('ptu_05.blessing_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'blessing_data_id'}
  messiah_effect: string
  messiah_frequency: string
  messiah_trigger?: (string) | null
  name: string
  page_num: number
  prerequisites?: (string) | null
  rank: number
  signer_effect: string
  signer_frequency: string
  signer_trigger?: (string) | null
  target?: (string) | null
  trigger?: (string) | null
}
export type {BlessingData_InsertParameters}