/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: XiMzv2k13SZNTx+Px7muYLsa/SbbWC6xWnglbWKWUY1H30ETRwGfx7mc9oAoMVzgmGPPW0FiVzo+ofmuEvPUkA==
 */

/* eslint-disable */
// tslint:disable

interface SkillData {
  book: string
  /**
   * @default nextval('pta_03.skill_data_id_seq'::regclass)
   */
  id: number & {readonly __brand?: 'skill_data_id'}
  long_effect: string
  name: string
  page_num: number
  short_effect: string
}
export default SkillData;

interface SkillData_InsertParameters {
  book: string
  /**
   * @default nextval('pta_03.skill_data_id_seq'::regclass)
   */
  id?: number & {readonly __brand?: 'skill_data_id'}
  long_effect: string
  name: string
  page_num: number
  short_effect: string
}
export type {SkillData_InsertParameters}
