import { iconFiles } from "$lib/legacy/flows/dappsExplorer/dapps";

describe("dapps", () => {
  it("all logos are present", async () => {
    (await import("./dapps.json")).default.forEach((dapp) => {
      expect(
        Object.keys(iconFiles).find((path) => path.endsWith(dapp.logo)),
        `The file ${dapp.logo} is missing`,
      ).toBeDefined();
    });
  });
});
