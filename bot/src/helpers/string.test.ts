import * as fc from "fast-check";
import { extractCharacters } from "./string";

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
        (firstStr, rest) => {
          const fullStr =
            firstStr +
            rest
              .map(
                ([{ name, prop }, nextStr]) => `\$${name}:${prop}\$${nextStr}`
              )
              .join("");
          const result = extractCharacters(fullStr, ["$"], [":"]);
          expect(result).toEqual(rest.map(([c]) => c));
        }
      )
    ));
  it("can parse messages without urls, with arbitrary charsign and charsep", () =>
    fc.assert(
      fc.property(
        fc
          .set(fc.string({ minLength: 1 }), { minLength: 2, maxLength: 20 })
          .chain((mixedSymbols) => {
            const filtered = mixedSymbols.filter((s1) =>
              mixedSymbols.every((s2) => s1 === s2 || !s2.includes(s1))
            );
            const strGen = stringWithout(filtered);
            return fc
              .integer({ min: 1, max: filtered.length - 1 })
              .chain((splitIndex) => {
                const signs = filtered.slice(0, splitIndex);
                const seps = filtered.slice(splitIndex);
                return fc.tuple(
                  fc.constant(signs),
                  fc.constant(seps),
                  strGen,
                  fc.array(fc.tuple(character(strGen), strGen))
                );
              });
          }),
        ([signs, seps, firstStr, rest]) => {
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
          const result = extractCharacters(fullStr, signs, seps);
          expect(result).toEqual(rest.map(([c]) => c));
        }
      )
    ));
});
