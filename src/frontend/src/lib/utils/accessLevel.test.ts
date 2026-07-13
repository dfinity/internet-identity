import {
  readAccessLevelPreference,
  toPermissionsArg,
  writeAccessLevelPreference,
  type AccessLevel,
  type AccessLevelFlow,
} from "./accessLevel";

describe("toPermissionsArg", () => {
  it("maps read-only to an explicit queries variant", () => {
    expect(toPermissionsArg("read-only")).toEqual([{ queries: null }]);
  });

  it("maps full-access to an explicit all variant", () => {
    expect(toPermissionsArg("full-access")).toEqual([{ all: null }]);
  });
});

describe("access-level preference persistence", () => {
  const flows: AccessLevelFlow[] = ["continue", "cli", "mcp"];
  const ANCHOR = BigInt(10_000);
  const OTHER_ANCHOR = BigInt(20_000);

  beforeEach(() => {
    localStorage.clear();
  });

  it("returns undefined when nothing has been stored (first-time sign in)", () => {
    for (const flow of flows) {
      expect(readAccessLevelPreference(flow, ANCHOR)).toBeUndefined();
    }
  });

  it("round-trips a stored preference", () => {
    for (const level of ["read-only", "full-access"] as AccessLevel[]) {
      writeAccessLevelPreference("continue", ANCHOR, level);
      expect(readAccessLevelPreference("continue", ANCHOR)).toBe(level);
    }
  });

  it("keeps preferences separate across flows", () => {
    writeAccessLevelPreference("continue", ANCHOR, "full-access");
    writeAccessLevelPreference("cli", ANCHOR, "read-only");
    // mcp is left untouched.
    expect(readAccessLevelPreference("continue", ANCHOR)).toBe("full-access");
    expect(readAccessLevelPreference("cli", ANCHOR)).toBe("read-only");
    expect(readAccessLevelPreference("mcp", ANCHOR)).toBeUndefined();
  });

  it("keeps preferences separate across anchors (shared-device safety)", () => {
    // One anchor's choice must not become another anchor's default on a
    // shared/public browser.
    writeAccessLevelPreference("mcp", ANCHOR, "read-only");
    expect(readAccessLevelPreference("mcp", OTHER_ANCHOR)).toBeUndefined();
    writeAccessLevelPreference("mcp", OTHER_ANCHOR, "full-access");
    expect(readAccessLevelPreference("mcp", ANCHOR)).toBe("read-only");
    expect(readAccessLevelPreference("mcp", OTHER_ANCHOR)).toBe("full-access");
  });

  it("ignores an unrecognized stored value", () => {
    localStorage.setItem(`ii:access-level:continue:${ANCHOR}`, "garbage");
    expect(readAccessLevelPreference("continue", ANCHOR)).toBeUndefined();
  });
});
