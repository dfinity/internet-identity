import { describe, expect, it } from "vitest";
import {
  IDENTITY_PALETTES,
  identityInitials,
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

describe("identityInitials", () => {
  it.each([
    ["Thomas", "TH"],
    ["Testing", "TE"],
    ["work", "WO"],
    ["Ada Lovelace", "AL"],
    ["Mary Ann Evans", "ME"],
    ["  émile zola ", "ÉZ"],
    ["ana@dfinity.org", "AN"],
    ["X", "X"],
    ["Тест 2", "ТЕ"],
  ])("takes %j as %j", (name, initials) => {
    expect(identityInitials(name)).toBe(initials);
  });

  it.each([undefined, "", "123", "🙂"])("has none for %j", (name) => {
    expect(identityInitials(name)).toBeUndefined();
  });
});
