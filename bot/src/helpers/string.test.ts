import * as fc from "fast-check";
import { extractCharacters, parseAttributes } from "./string";

const stringWithout = (
  excluded: string[],
  options?: fc.StringSharedConstraints
) =>
  fc
    .stringOf(fc.char(), options ?? {})
    .filter((s) => excluded.every((e) => !s.includes(e)));
const character = (str: fc.Arbitrary<string>) =>
  fc.record({ name: str, prop: str });

describe("extractCharacters", () => {
  it("can parse messages without urls, with default charsign & charsep", () =>
    fc.assert(
      fc.property(
        stringWithout(["$", ":"]),
        fc.array(
          fc.tuple(
            character(stringWithout(["$", ":"])),
            stringWithout(["$", ":"])
          )
        ),
        fc.context(),
        (firstStr, rest, ctx) => {
          const fullStr =
            firstStr +
            rest
              .map(
                ([{ name, prop }, nextStr]) => `\$${name}:${prop}\$${nextStr}`
              )
              .join("");
          const result = extractCharacters(fullStr, ["$"], [":"]);
          ctx.log(fullStr);
          expect(result).toEqual(rest.map(([c]) => c));
        }
      )
    ));
  it("can parse messages without urls, with arbitrary charsign and charsep", () =>
    fc.assert(
      fc.property(
        fc
          .set(stringWithout([" "], { minLength: 1 }), {
            minLength: 2,
            maxLength: 20,
          })
          .chain((mixedSymbols) => {
            const filtered = mixedSymbols.filter((s1) =>
              mixedSymbols.every((s2) => s1 === s2 || !s2.includes(s1))
            );
            const sanitized = filtered.length < 2 ? ["$", ":"] : filtered;
            const strGen = stringWithout(sanitized, { minLength: 1 });
            return fc
              .integer({ min: 1, max: sanitized.length - 1 })
              .map((splitIndex) => ({
                signs: sanitized.slice(0, splitIndex),
                seps: sanitized.slice(splitIndex),
              }))
              .chain(({ signs, seps }) =>
                fc.record({
                  signs: fc.constant(signs),
                  seps: fc.constant(seps),
                  firstStr: strGen,
                  rest: fc.array(fc.tuple(character(strGen), strGen)),
                })
              );
          }),
        fc.context(),
        ({ signs, seps, firstStr, rest }, ctx) => {
          const fullStr =
            firstStr +
            rest
              .map(
                ([{ name, prop }, nextStr]) =>
                  `${signs[Math.floor(Math.random() * signs.length)]}${name}${
                    seps[Math.floor(Math.random() * seps.length)]
                  }${prop}${
                    signs[Math.floor(Math.random() * signs.length)]
                  }${nextStr}`
              )
              .join("");
          ctx.log(fullStr);
          const result = extractCharacters(fullStr, signs, seps);
          expect(result).toEqual(rest.map(([c]) => c));
        }
      )
    ));
});

const re = /["\s=\\]/;
const printStr = (str: string) =>
  re.test(str) ? `"${str.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"` : str;
const printDictionary = (dict: Record<string, string | null>) =>
  Object.entries(dict)
    .map(([key, value]) =>
      value === null ? printStr(key) : `${printStr(key)}=${printStr(value)}`
    )
    .join(" ");

describe("parseAttributes", () => {
  it("undoes printDictionary", () =>
    fc.assert(
      fc.property(
        fc.dictionary(fc.string({ minLength: 1 }), fc.string({ minLength: 1 })),
        fc.context(),
        (dict, ctx) => {
          const printed = printDictionary(dict);
          ctx.log(printed);
          expect(parseAttributes(printed)).toEqual(dict);
        }
      ),
      { numRuns: 1000 }
    ));
});
