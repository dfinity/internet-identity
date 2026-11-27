import { describe, expect, it } from "vitest";
import {
  IDENTITY_PALETTES,
  identityInitial,
  identityPalette,
} from "./identityColors";

describe("identityPalette", () => {
  it("gives an identity the same colours every time", () => {
    expect(identityPalette(BigInt(10_042))).toBe(
      identityPalette(BigInt(10_042)),
    );
  });

  it("spreads consecutive identity numbers over the palettes", () => {
    const used = new Set(
      Array.from({ length: 48 }, (_, i) => identityPalette(BigInt(10_000 + i))),
    );
    expect(used.size).toBeGreaterThanOrEqual(IDENTITY_PALETTES.length * 0.6);
  });
});

describe("identityInitial", () => {
  it.each([
    ["Thomas", "T"],
    ["work", "W"],
    ["Ada Lovelace", "A"],
    ["  émile zola ", "É"],
    ["e\u0301mile", "E\u0301"],
    ["ana@dfinity.org", "A"],
    ["X", "X"],
    ["Тест 2", "Т"],
    ["2nd account", "N"],
  ])("takes %j as %j", (name, initial) => {
    expect(identityInitial(name)).toBe(initial);
  });

  it.each([undefined, "", "123", "🙂", "@dfinity.org"])(
    "has none for %j",
    (name) => {
      expect(identityInitial(name)).toBeUndefined();
    },
  );
});
