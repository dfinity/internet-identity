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

    const isHttp = (url: URL): boolean =>
      url.protocol === "https:" || url.protocol === "http:";

    const invalid: string[] = [];
    for (const dapp of dapps) {
      // `website` is a display link, so it only needs to be a valid http(s)
      // URL — it may legitimately carry a path or trailing slash.
      try {
        if (!isHttp(new URL(dapp.website))) {
          invalid.push(`${dapp.name} (${dapp.website}): website not http(s)`);
        }
      } catch {
        invalid.push(`${dapp.name} (${dapp.website}): website not a valid URL`);
      }

      // `authOrigins` are matched at runtime via `new URL(authOrigin).origin`,
      // so each must be a bare http(s) origin — a path, query, or fragment
      // would never match and is a mistake.
      for (const authOrigin of authOriginsOf(dapp)) {
        try {
          const url = new URL(authOrigin);
          if (!isHttp(url) || url.origin !== authOrigin) {
            invalid.push(`${dapp.name} (${authOrigin}): not a bare origin`);
          }
        } catch {
          invalid.push(`${dapp.name} (${authOrigin}): not a valid URL`);
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
