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

  it("all website and authOrigins are valid origins", async () => {
    const dapps = (await import("./dapps.json")).default;

    // `authOrigins` is either a single origin string or an array of them.
    const authOriginsOf = (dapp: (typeof dapps)[number]): string[] => {
      const value = (dapp as { authOrigins?: string | string[] }).authOrigins;
      if (value === undefined) {
        return [];
      }
      return typeof value === "string" ? [value] : value;
    };

    // Every dapp must list at least one `authOrigins` entry — even a
    // single-domain dapp must state the origin it authenticates from, rather
    // than relying on `website` (which is a display link and may carry a path
    // or trailing slash).
    const missingAuthOrigins = dapps
      .filter((dapp) => authOriginsOf(dapp).length === 0)
      .map((dapp) => dapp.name);

    // `authOrigins` are matched at runtime via `new URL(origin).origin`, so
    // each must be a valid http(s) URL. `website` is also checked since it is
    // used as a link and, when no dedicated match exists, for origin matching.
    const invalid: string[] = [];
    for (const dapp of dapps) {
      for (const origin of [dapp.website, ...authOriginsOf(dapp)]) {
        try {
          const { protocol } = new URL(origin);
          if (protocol !== "https:" && protocol !== "http:") {
            invalid.push(`${dapp.name} (${origin}): not http(s)`);
          }
        } catch {
          invalid.push(`${dapp.name} (${origin}): not a valid URL`);
        }
      }
    }

    expect(
      { invalid, missingAuthOrigins },
      [
        invalid.length > 0 && `Invalid origins:\n  ${invalid.join("\n  ")}`,
        missingAuthOrigins.length > 0 &&
          `Missing authOrigins:\n  ${missingAuthOrigins.join("\n  ")}`,
      ]
        .filter(Boolean)
        .join("\n"),
    ).toEqual({ invalid: [], missingAuthOrigins: [] });
  });
});
