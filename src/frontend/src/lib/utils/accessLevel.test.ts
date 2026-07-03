import {
  isToggleChecked,
  toAccessLevelArg,
  toggledAccessLevel,
  type AccessLevel,
} from "./accessLevel";

describe("toAccessLevelArg", () => {
  it("maps read-only to an explicit read_only variant", () => {
    expect(toAccessLevelArg("read-only")).toEqual([{ read_only: null }]);
  });

  it("maps full-access to an explicit full_access variant", () => {
    expect(toAccessLevelArg("full-access")).toEqual([{ full_access: null }]);
  });
});

// The toggle mapping is a double inversion (which level the box offers vs.
// which is selected), so pin all combinations: a silently flipped default
// here would grant the wrong privilege by default.
describe("access-level toggle mapping", () => {
  const levels: AccessLevel[] = ["read-only", "full-access"];

  it("is ticked exactly when the current level is the prompted one", () => {
    for (const prompt of levels) {
      for (const accessLevel of levels) {
        expect(isToggleChecked(accessLevel, prompt)).toBe(
          accessLevel === prompt,
        );
      }
    }
  });

  it("selects the prompted level when ticked", () => {
    expect(toggledAccessLevel(true, "read-only")).toBe("read-only");
    expect(toggledAccessLevel(true, "full-access")).toBe("full-access");
  });

  it("falls back to the other level when unticked", () => {
    expect(toggledAccessLevel(false, "read-only")).toBe("full-access");
    expect(toggledAccessLevel(false, "full-access")).toBe("read-only");
  });

  it("round-trips: untick then tick restores the prompted level", () => {
    for (const prompt of levels) {
      expect(toggledAccessLevel(true, prompt)).toBe(prompt);
      expect(
        toggledAccessLevel(
          isToggleChecked(toggledAccessLevel(false, prompt), prompt),
          prompt,
        ),
      ).not.toBe(prompt);
    }
  });
});
