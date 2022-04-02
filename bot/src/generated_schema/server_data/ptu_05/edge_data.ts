/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: Go50AxexVlG0P9pRNj5Mp3idIOvijSaKfb3yhDwM+nefohvOBwYUYvwjVa3Z9ilj/dQQ+nKZs/EQHTTgigSpVg==
 */

/* eslint-disable */
// tslint:disable

interface EdgeData {
  book: string
  effect: string
  frequency: (string) | null
  /**
   * @default nextval('ptu_05.edge_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'edge_data_id'}
  name: string
  page_num: number
  prerequisites: (string) | null
}
export default EdgeData;

interface EdgeData_InsertParameters {
  book: string
  effect: string
  frequency?: (string) | null
  /**
   * @default nextval('ptu_05.edge_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'edge_data_id'}
  name: string
  page_num: number
  prerequisites?: (string) | null
}
export type {EdgeData_InsertParameters}