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

  beforeEach(() => {
    localStorage.clear();
  });

  it("returns undefined when nothing has been stored (first-time sign in)", () => {
    for (const flow of flows) {
      expect(readAccessLevelPreference(flow)).toBeUndefined();
    }
  });

  it("round-trips a stored preference", () => {
    for (const level of ["read-only", "full-access"] as AccessLevel[]) {
      writeAccessLevelPreference("continue", level);
      expect(readAccessLevelPreference("continue")).toBe(level);
    }
  });

  it("keeps preferences separate across flows", () => {
    writeAccessLevelPreference("continue", "full-access");
    writeAccessLevelPreference("cli", "read-only");
    // mcp is left untouched.
    expect(readAccessLevelPreference("continue")).toBe("full-access");
    expect(readAccessLevelPreference("cli")).toBe("read-only");
    expect(readAccessLevelPreference("mcp")).toBeUndefined();
  });

  it("ignores an unrecognized stored value", () => {
    localStorage.setItem("ii:access-level:continue", "garbage");
    expect(readAccessLevelPreference("continue")).toBeUndefined();
  });
});
