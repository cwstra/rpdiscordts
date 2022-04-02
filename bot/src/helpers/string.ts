import { zipWith } from "fp-ts/lib/Array";
import { splitEvery } from "rambda";
import { last } from "./array";

const reEscape = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

const makeSymbolReGroup = (symbols: string[]) =>
  `(?:${symbols.map(reEscape).join("|")})`;

export function extractCharacters(
  message: string,
  charsigns: string[],
  charseps: string[]
) {
  const signPart = makeSymbolReGroup(charsigns);
  const sepPart = makeSymbolReGroup(charseps);
  return Array.from(
    message
      //TODO?: Replace this with urlRegexSafe() if this is insufficient
      .replace(/https?:\/\/[^\s]*\.[^\s]*/g, "")
      .matchAll(new RegExp(`${signPart}(.*?)${sepPart}(.*?)${signPart}`, "g"))
  ).map((a) => ({ name: a[1], prop: a[2] }));
}

export function midStringCharacters(
  message: string,
  charsigns: string[],
  charseps: string[]
): [string, ...[{ name: string; prop: string }, string][]] {
  const signPart = makeSymbolReGroup(charsigns);
  const sepPart = makeSymbolReGroup(charseps);
  const splits = message
    //TODO?: Replace this with urlRegexSafe() if this is insufficient
    .replace(/https?:\/\/[^\s]*\.[^\s]*/g, "")
    .split(new RegExp(`${signPart}(.*?)${sepPart}(.*?)${signPart}`));
  const [first, ...rest] = splits;
  return [
    first,
    ...splitEvery(3, rest).map(
      (s): [{ name: string; prop: string }, string] => [
        { name: s[0], prop: s[1] },
        s[2],
      ]
    ),
  ];
}

export function stringListing(items: string[]) {
  if (items.length < 3) return items.join(" and ");
  return `${items.slice(0, -1).join(", ")}, and ${last(items)}`;
}

export const trimmedMultiLine = (
  [h, ...strings]: TemplateStringsArray,
  ...interpolated: { toString: () => string }[]
) => {
  const wholeString = [
    h,
    ...zipWith(strings, interpolated, (s, t) => t.toString() + s),
  ].join("");
  return wholeString.replace(/\s*\n\s*/g, (s) => s.replace(/[^\n]/g, ""));
};
