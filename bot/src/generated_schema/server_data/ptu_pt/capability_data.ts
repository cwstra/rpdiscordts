/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: mfamFxo+4tdwYm7FomtAHiet1UPHWVtOhh68RErA7r2aI6tseZrK54UYxeVVH9PdL6fqg/6hmM8FYEUTBcfD9g==
 */

/* eslint-disable */
// tslint:disable

interface CapabilityData {
  book: string
  description: string
  fields: (any) | null
  /**
   * @default nextval('ptu_pt.capability_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'capability_data_id'}
  image: (string) | null
  name: string
  page_num: number
}
export default CapabilityData;

interface CapabilityData_InsertParameters {
  book: string
  description: string
  fields?: (any) | null
  /**
   * @default nextval('ptu_pt.capability_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'capability_data_id'}
  image?: (string) | null
  name: string
  page_num: number
}
export type {CapabilityData_InsertParameters}