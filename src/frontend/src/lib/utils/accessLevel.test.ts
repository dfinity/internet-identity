import { toPermissionsArg } from "./accessLevel";

describe("toPermissionsArg", () => {
  it("maps read-only to an explicit queries variant", () => {
    expect(toPermissionsArg("read-only")).toEqual([{ queries: null }]);
  });

  it("maps full-access to an explicit all variant", () => {
    expect(toPermissionsArg("full-access")).toEqual([{ all: null }]);
  });
});
