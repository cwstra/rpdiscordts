/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: IWYon0sDUeNzxBLnHZ6w0k6l/5movJCA27OcwS/VikQTMjJRzH2gKopRTBFZwgxTuchZRSFcvN8SO/rECnVy4g==
 */

/* eslint-disable */
// tslint:disable

import AbilityData, {AbilityData_InsertParameters} from './ability_data'
import AmbiguiousNames, {AmbiguiousNames_InsertParameters} from './ambiguious_names'
import CapabilityData, {CapabilityData_InsertParameters} from './capability_data'
import Feature, {Feature_InsertParameters} from './feature'
import Item, {Item_InsertParameters} from './item'
import List, {List_InsertParameters} from './list'
import Maneuver, {Maneuver_InsertParameters} from './maneuver'
import Minifeat, {Minifeat_InsertParameters} from './minifeat'
import MonNature, {MonNature_InsertParameters} from './mon_nature'
import Move, {Move_InsertParameters} from './move'
import Pokeedge, {Pokeedge_InsertParameters} from './pokeedge'
import Pokemon, {Pokemon_InsertParameters} from './pokemon'
import Schema, {Schema_InsertParameters} from './schema'
import Status, {Status_InsertParameters} from './status'

interface DatabaseSchema {
  ability_data: {record: AbilityData, insert: AbilityData_InsertParameters};
  ambiguious_names: {record: AmbiguiousNames, insert: AmbiguiousNames_InsertParameters};
  capability_data: {record: CapabilityData, insert: CapabilityData_InsertParameters};
  feature: {record: Feature, insert: Feature_InsertParameters};
  item: {record: Item, insert: Item_InsertParameters};
  list: {record: List, insert: List_InsertParameters};
  maneuver: {record: Maneuver, insert: Maneuver_InsertParameters};
  minifeat: {record: Minifeat, insert: Minifeat_InsertParameters};
  mon_nature: {record: MonNature, insert: MonNature_InsertParameters};
  move: {record: Move, insert: Move_InsertParameters};
  pokeedge: {record: Pokeedge, insert: Pokeedge_InsertParameters};
  pokemon: {record: Pokemon, insert: Pokemon_InsertParameters};
  schema: {record: Schema, insert: Schema_InsertParameters};
  status: {record: Status, insert: Status_InsertParameters};
}
export default DatabaseSchema;

/**
 * JSON serialize values (v) if the table name (t) and column name (c)
 * is a JSON or JSONB column.
 * This is necessary if you want to store values that are not plain objects
 * in a JSON or JSONB column.
 */
function serializeValue(t: string, c: string, v: unknown): unknown {
  if (
    (t === "capability_data" && c === "fields") ||
    (t === "feature" && c === "embed") ||
    (t === "item" && c === "embed") ||
    (t === "list" && c === "embed") ||
    (t === "maneuver" && c === "embed") ||
    (t === "minifeat" && c === "embed") ||
    (t === "mon_nature" && c === "embed") ||
    (t === "move" && c === "embed") ||
    (t === "pokeedge" && c === "embed") ||
    (t === "pokemon" && c === "embed") ||
    (t === "schema" && c === "schema") ||
    (t === "status" && c === "embed")
  ) {
    return JSON.stringify(v);
  }
  return v;
}
export {serializeValue}

export type {
  AbilityData,
  AbilityData_InsertParameters,
  AmbiguiousNames,
  AmbiguiousNames_InsertParameters,
  CapabilityData,
  CapabilityData_InsertParameters,
  Feature,
  Feature_InsertParameters,
  Item,
  Item_InsertParameters,
  List,
  List_InsertParameters,
  Maneuver,
  Maneuver_InsertParameters,
  Minifeat,
  Minifeat_InsertParameters,
  MonNature,
  MonNature_InsertParameters,
  Move,
  Move_InsertParameters,
  Pokeedge,
  Pokeedge_InsertParameters,
  Pokemon,
  Pokemon_InsertParameters,
  Schema,
  Schema_InsertParameters,
  Status,
  Status_InsertParameters,
}
