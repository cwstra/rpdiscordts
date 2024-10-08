/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: ogT0M95vFGshX7K+9MSysbuKD5OjTdcU7IaBYQlFxuXP3QV3T88rz6OFKRXJmyKu6O944y5dBauwxxRSsxwD3w==
 */

/* eslint-disable */
// tslint:disable

interface ItemData {
  book: string
  cost: (string) | null
  description: string
  /**
   * @default nextval('ptu_pt.item_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'item_data_id'}
  modifier: (string) | null
  name: string
  page_num: string
  slot: (string) | null
}
export default ItemData;

interface ItemData_InsertParameters {
  book: string
  cost?: (string) | null
  description: string
  /**
   * @default nextval('ptu_pt.item_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'item_data_id'}
  modifier?: (string) | null
  name: string
  page_num: string
  slot?: (string) | null
}
export type {ItemData_InsertParameters}
