import { zipWith } from "fp-ts/lib/Array";
import { fromPairs, splitEvery } from "rambda";
import { last } from "./array";
import {
  alt,
  apply,
  rep_sc,
  tok as genTok,
  seq,
  buildLexer,
  list_sc,
  expectSingleResult,
  expectEOF,
} from "typescript-parsec";

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

export const parseAttributes = (() => {
  type TokenKind = "simple" | "escaped" | "quote" | "eq" | "space";
  const lexer = buildLexer<TokenKind>([
    [true, /^[^\\"=\s]+/g, "simple"],
    [true, /^\\./g, "escaped"],
    [true, /^"/g, "quote"],
    [true, /^=/g, "eq"],
    [true, /^\s+/g, "space"],
  ]);
  const tok = (toMatch: TokenKind) => genTok(toMatch);
  const simple = apply(tok("simple"), (t) => t.text);
  const escaped = apply(tok("escaped"), (t) =>
    ['"', "\\"].includes(t.text[1]) ? t.text[1] : t.text
  );
  const quoted = apply(
    seq(
      tok("quote"),
      apply(
        rep_sc(
          alt(
            simple,
            escaped,
            apply(tok("eq"), () => "="),
            apply(tok("space"), (s) => s.text)
          )
        ),
        (strs) => strs.join("")
      ),
      tok("quote")
    ),
    ([, str]) => str
  );
  const identifier = alt(quoted, simple);
  const pair = apply(
    seq(identifier, tok("eq"), identifier),
    ([name, , value]): [string, string | null] => [name, value]
  );
  const single = apply(identifier, (id): [string, string | null] => [id, null]);
  const sequence = apply(list_sc(alt(pair, single), tok("space")), (pairs) =>
    fromPairs(pairs)
  );
  return (expr: string): Record<string, string | null> => {
    const trimmed = expr.trim();
    return trimmed
      ? expectSingleResult(expectEOF(sequence.parse(lexer.parse(trimmed))))
      : {};
  };
})();
