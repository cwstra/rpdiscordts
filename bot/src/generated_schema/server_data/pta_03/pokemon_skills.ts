/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: WJgesHjz0bw9Eyk9iD9920TbR07uTUecgFbBEA2nBgBfGr+sKM9Eph/MDxfdHLWKSeVmI29t5H3b0oRf3sTSaA==
 */

/* eslint-disable */
// tslint:disable

interface PokemonSkills {
  pokemon_id: number & {readonly __brand?: 'pokemon_skills_pokemon_id'}
  skill_id: number & {readonly __brand?: 'pokemon_skills_skill_id'}
}
export default PokemonSkills;

interface PokemonSkills_InsertParameters {
  pokemon_id: number & {readonly __brand?: 'pokemon_skills_pokemon_id'}
  skill_id: number & {readonly __brand?: 'pokemon_skills_skill_id'}
}
export type {PokemonSkills_InsertParameters}
