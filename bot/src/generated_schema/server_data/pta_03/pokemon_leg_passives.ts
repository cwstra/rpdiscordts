/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: G/i6Q60CfxCM6J+LMNPfinBvs8fVksNFQM8t+kAaNGCWPCNklw778eumCILwTEazGvIYfH0YoUWKG6AmMJuixw==
 */

/* eslint-disable */
// tslint:disable

import LegPassiveData from './leg_passive_data'

interface PokemonLegPassives {
  leg_passive_id: LegPassiveData['id']
  pokemon_id: number & {readonly __brand?: 'pokemon_leg_passives_pokemon_id'}
}
export default PokemonLegPassives;

interface PokemonLegPassives_InsertParameters {
  leg_passive_id: LegPassiveData['id']
  pokemon_id: number & {readonly __brand?: 'pokemon_leg_passives_pokemon_id'}
}
export type {PokemonLegPassives_InsertParameters}
