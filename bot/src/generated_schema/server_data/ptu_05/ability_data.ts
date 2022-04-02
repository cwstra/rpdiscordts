/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: 7vbA3a4MLvptf3TCiZmwhI4qEBvXBjE/CPWuIHpfifYHIKiE5aqvNA2+OQVcBvRQuCwZMonIzY7nHcEFvCzIwA==
 */

/* eslint-disable */
// tslint:disable

interface AbilityData {
  book: string
  effect: string
  frequency: string
  /**
   * @default nextval('ptu_05.ability_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'ability_data_id'}
  keywords: (string) | null
  name: string
  page_num: number
  target: (string) | null
  trigger: (string) | null
}
export default AbilityData;

interface AbilityData_InsertParameters {
  book: string
  effect: string
  frequency: string
  /**
   * @default nextval('ptu_05.ability_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'ability_data_id'}
  keywords?: (string) | null
  name: string
  page_num: number
  target?: (string) | null
  trigger?: (string) | null
}
export type {AbilityData_InsertParameters}