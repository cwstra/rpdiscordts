/**
 * !!! This file is autogenerated do not edit by hand !!!
 *
 * Generated by: @databases/pg-schema-print-types
 * Checksum: 1ufcfecyYaxAf3GHPOCY6pgALPpHgPxQu/MkhsA/mTNLxKTlppHWxluVzGsAaLuZzOlUtNyWmYNQEwwIJh4gVw==
 */

/* eslint-disable */
// tslint:disable

import Amenity, {Amenity_InsertParameters} from './amenity'
import Basic, {Basic_InsertParameters} from './basic'
import Building, {Building_InsertParameters} from './building'
import Enchantment, {Enchantment_InsertParameters} from './enchantment'
import Feats, {Feats_InsertParameters} from './feats'
import Items, {Items_InsertParameters} from './items'
import List, {List_InsertParameters} from './list'
import Maneuvers, {Maneuvers_InsertParameters} from './maneuvers'
import Race, {Race_InsertParameters} from './race'
import Schema, {Schema_InsertParameters} from './schema'
import Song, {Song_InsertParameters} from './song'
import Spells, {Spells_InsertParameters} from './spells'
import Status, {Status_InsertParameters} from './status'
import Techs, {Techs_InsertParameters} from './techs'
import Terrain, {Terrain_InsertParameters} from './terrain'
import Weather, {Weather_InsertParameters} from './weather'

interface DatabaseSchema {
  amenity: {record: Amenity, insert: Amenity_InsertParameters};
  basic: {record: Basic, insert: Basic_InsertParameters};
  building: {record: Building, insert: Building_InsertParameters};
  enchantment: {record: Enchantment, insert: Enchantment_InsertParameters};
  feats: {record: Feats, insert: Feats_InsertParameters};
  items: {record: Items, insert: Items_InsertParameters};
  list: {record: List, insert: List_InsertParameters};
  maneuvers: {record: Maneuvers, insert: Maneuvers_InsertParameters};
  race: {record: Race, insert: Race_InsertParameters};
  schema: {record: Schema, insert: Schema_InsertParameters};
  song: {record: Song, insert: Song_InsertParameters};
  spells: {record: Spells, insert: Spells_InsertParameters};
  status: {record: Status, insert: Status_InsertParameters};
  techs: {record: Techs, insert: Techs_InsertParameters};
  terrain: {record: Terrain, insert: Terrain_InsertParameters};
  weather: {record: Weather, insert: Weather_InsertParameters};
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
    (t === "amenity" && c === "embed") ||
    (t === "basic" && c === "embed") ||
    (t === "building" && c === "embed") ||
    (t === "enchantment" && c === "embed") ||
    (t === "feats" && c === "embed") ||
    (t === "items" && c === "embed") ||
    (t === "list" && c === "embed") ||
    (t === "maneuvers" && c === "embed") ||
    (t === "race" && c === "embed") ||
    (t === "schema" && c === "schema") ||
    (t === "song" && c === "embed") ||
    (t === "spells" && c === "embed") ||
    (t === "status" && c === "embed") ||
    (t === "techs" && c === "embed") ||
    (t === "terrain" && c === "embed") ||
    (t === "weather" && c === "embed")
  ) {
    return JSON.stringify(v);
  }
  return v;
}
export {serializeValue}

export type {
  Amenity,
  Amenity_InsertParameters,
  Basic,
  Basic_InsertParameters,
  Building,
  Building_InsertParameters,
  Enchantment,
  Enchantment_InsertParameters,
  Feats,
  Feats_InsertParameters,
  Items,
  Items_InsertParameters,
  List,
  List_InsertParameters,
  Maneuvers,
  Maneuvers_InsertParameters,
  Race,
  Race_InsertParameters,
  Schema,
  Schema_InsertParameters,
  Song,
  Song_InsertParameters,
  Spells,
  Spells_InsertParameters,
  Status,
  Status_InsertParameters,
  Techs,
  Techs_InsertParameters,
  Terrain,
  Terrain_InsertParameters,
  Weather,
  Weather_InsertParameters,
}
