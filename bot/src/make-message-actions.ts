import { Str } from "@rimbu/typical";
import {
  BuildTuple,
  Digit,
  DigitToTup,
  FromNumber,
  Subtract,
} from "@rimbu/typical/dist/types/strnum";
import {
  EmojiIdentifierResolvable,
  MessageActionRow,
  MessageButton,
  MessageButtonStyleResolvable,
  MessageSelectMenu,
  MessageSelectOptionData,
} from "discord.js";

type DigitToRange = {
  "0": "0";
  "1": "0" | "1";
  "2": "0" | "1" | "2";
  "3": "0" | "1" | "2" | "3";
  "4": "0" | "1" | "2" | "3" | "4";
  "5": "0" | "1" | "2" | "3" | "4" | "5";
  "6": "0" | "1" | "2" | "3" | "4" | "5" | "6";
  "7": "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7";
  "8": "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8";
  "9": "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
};
type RangeToTuple<
  Range extends Digit,
  Tup extends unknown[]
> = Range extends Digit ? DigitToTup<Tup>[Range] : never;
type JoinTuples<T1 extends unknown[], T2 extends unknown[]> = T1 extends []
  ? T2
  : T1 extends (infer E)[]
  ? T2 extends E[]
    ? [...T1, ...T2]
    : never
  : never;
type BuildTupleRange<
  N extends string,
  Deca extends unknown[] = [unknown]
> = N extends Str.Append<infer First, Digit>
  ? First extends ""
    ? RangeToTuple<DigitToRange[Digit & N], Deca>
    : N extends Str.Append<First, infer D>
    ?
        | JoinTuples<
            BuildTupleRange<Subtract<First, "1">, DigitToTup<Deca>["deca"]>,
            BuildTupleRange<"9", Deca>
          >
        | JoinTuples<
            BuildTuple<First, DigitToTup<Deca>["deca"]>,
            BuildTupleRange<D, Deca>
          >
    : never
  : never;
type SRangeTuple<T, Min extends string, Max extends string> = Subtract<
  Max,
  Min
> extends never
  ? never
  : JoinTuples<BuildTuple<Min, [T]>, BuildTupleRange<Subtract<Max, Min>, [T]>>;
export type RangeTuple<T, Min extends number, Max extends number> = SRangeTuple<
  T,
  FromNumber<Min>,
  FromNumber<Max>
>;

type ButtonArg =
  | {
      customId: string;
      label: string;
      style: Exclude<MessageButtonStyleResolvable, "LINK">;
      disabled?: boolean;
      emoji?: EmojiIdentifierResolvable;
    }
  | {
      customId: string;
      label: string;
      style: "LINK";
      url: string;
      disabled?: boolean;
      emoji?: EmojiIdentifierResolvable;
    };
type StringRange<SMin extends string, SMax extends string> = SRangeTuple<
  unknown,
  SMin,
  SMax
>["length"];
type ValidMaximums<
  Min extends number,
  PossibleMaximums
> = PossibleMaximums extends number ? [Min, PossibleMaximums] : never;
type ValidRangesH<
  PossibleMinimums extends number,
  MaxMaximum extends string
> = PossibleMinimums extends number
  ? ValidMaximums<
      PossibleMinimums,
      StringRange<FromNumber<PossibleMinimums>, MaxMaximum>
    >
  : never;
type ValidRanges<MaxPossible extends number> = ValidRangesH<
  StringRange<"0", FromNumber<MaxPossible>>,
  FromNumber<MaxPossible>
>;
type SelectArg<Lengths> = Lengths extends number
  ? {
      customId: string;
      options: BuildTuple<FromNumber<Lengths>, [MessageSelectOptionData]>;
      placeholder?: string;
      disabled?: boolean;
      range?: ValidRanges<Lengths>;
    }
  : never;
type RowArg = RangeTuple<ButtonArg, 1, 5> | SelectArg<StringRange<"25", "25">>;

export const makeMessageActions = (
  arg: RangeTuple<RowArg, 1, 5>
): MessageActionRow[] =>
  arg.map((a) =>
    new MessageActionRow().addComponents(
      ...(Array.isArray(a)
        ? a.map((b) => {
            const button = new MessageButton()
              .setCustomId(b.customId)
              .setLabel(b.label)
              .setStyle(b.style);
            if (b.disabled !== undefined) button.setDisabled(b.disabled);
            if (b.emoji !== undefined) button.setEmoji(b.emoji);
            if (b.style === "LINK") button.setURL(b.url);
            return button;
          })
        : [
            (() => {
              const select = new MessageSelectMenu()
                .setCustomId(a.customId)
                .setOptions(a.options);
              if (a.placeholder !== undefined)
                select.setPlaceholder(a.placeholder);
              if (a.disabled !== undefined) select.setDisabled(a.disabled);
              if (a.range)
                select.setMinValues(a.range[0]).setMaxValues(a.range[1]);
              return select;
            })(),
          ])
    )
  );
