import { afterEach, describe, expect, it, vi } from "vitest";
import {
  allowedLink,
  fetchAlternativeOrigins,
  parseAlternativeOrigins,
} from "$lib/utils/notifications/worker/notificationLink";

const APP = "https://app.example";

describe("allowedLink", () => {
  it("keeps a link on the sending origin", () => {
    expect(
      allowedLink({
        url: `${APP}/chats/7`,
        origin: APP,
        alternativeOrigins: [],
      }),
    ).toBe(`${APP}/chats/7`);
  });

  it("keeps a link on an origin the app publishes as its own", () => {
    expect(
      allowedLink({
        url: "https://app.ic0.app/chats/7",
        origin: APP,
        alternativeOrigins: ["https://app.ic0.app"],
      }),
    ).toBe("https://app.ic0.app/chats/7");
  });

  it("keeps a canister's link on the gateway domain it is served on", () => {
    const canister = "vt36r-2qaaa-aaaad-aad5a-cai";
    expect(
      allowedLink({
        url: `https://${canister}.icp0.io/#chat`,
        // The origin a notification carries has been remapped onto ic0.app.
        origin: `https://${canister}.ic0.app`,
        alternativeOrigins: [],
      }),
    ).toBe(`https://${canister}.icp0.io/#chat`);
  });

  it("replaces another canister's link with the app itself", () => {
    expect(
      allowedLink({
        url: "https://un4fu-tqaaa-aaaab-qadjq-cai.icp0.io/#chat",
        origin: "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app",
        alternativeOrigins: [],
      }),
    ).toBe("https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net");
  });

  it("replaces a link the app does not vouch for with the app itself", () => {
    expect(
      allowedLink({
        url: "https://evil.example/drain",
        origin: APP,
        alternativeOrigins: ["https://app.ic0.app"],
      }),
    ).toBe(APP);
  });

  it("replaces a link that is no URL, and a missing one", () => {
    expect(
      allowedLink({
        url: "javascript:alert(1)",
        origin: APP,
        alternativeOrigins: [],
      }),
    ).toBe(APP);
    expect(
      allowedLink({ url: "not a url", origin: APP, alternativeOrigins: [] }),
    ).toBe(APP);
    expect(
      allowedLink({ url: undefined, origin: APP, alternativeOrigins: [] }),
    ).toBe(APP);
  });

  describe("opens a canister's own origin on icp.net where the link falls back", () => {
    const CANISTER_ORIGIN = "https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app";
    const CANISTER_APP_URL = "https://vt36r-2qaaa-aaaad-aad5a-cai.icp.net";

    it("for a missing link", () => {
      expect(
        allowedLink({
          url: undefined,
          origin: CANISTER_ORIGIN,
          alternativeOrigins: [],
        }),
      ).toBe(CANISTER_APP_URL);
    });

    it("for a link that is no URL", () => {
      expect(
        allowedLink({
          url: "not a url",
          origin: CANISTER_ORIGIN,
          alternativeOrigins: [],
        }),
      ).toBe(CANISTER_APP_URL);
    });

    it("for a link the app does not vouch for", () => {
      expect(
        allowedLink({
          url: "https://evil.example/drain",
          origin: CANISTER_ORIGIN,
          alternativeOrigins: [],
        }),
      ).toBe(CANISTER_APP_URL);
    });
  });

  it("falls back to a custom domain as it stands", () => {
    for (const url of [undefined, "not a url", "https://evil.example/drain"]) {
      expect(allowedLink({ url, origin: APP, alternativeOrigins: [] })).toBe(
        APP,
      );
    }
  });
});

describe("parseAlternativeOrigins", () => {
  it("reads the origins an app lists", () => {
    expect(
      parseAlternativeOrigins(
        '{"alternativeOrigins":["https://app.ic0.app","https://app.icp0.io"]}',
      ),
    ).toEqual(["https://app.ic0.app", "https://app.icp0.io"]);
  });

  it("drops entries that are not bare origins", () => {
    expect(
      parseAlternativeOrigins(
        '{"alternativeOrigins":["https://app.ic0.app/path","app.ic0.app","https://ok.example"]}',
      ),
    ).toEqual(["https://ok.example"]);
  });

  it("reads nothing from a document it cannot use", () => {
    expect(parseAlternativeOrigins("not json")).toEqual([]);
    expect(parseAlternativeOrigins("[]")).toEqual([]);
    expect(parseAlternativeOrigins('{"other":[]}')).toEqual([]);
    expect(
      parseAlternativeOrigins(
        JSON.stringify({
          alternativeOrigins: Array(101).fill("https://ok.example"),
        }),
      ),
      // More than II accepts elsewhere for the same document.
    ).toEqual([]);
  });
});

describe("fetchAlternativeOrigins", () => {
  const canister = "vt36r-2qaaa-aaaad-aad5a-cai";
  const suiteFrontend = "https://mail.suite.example";

  /** An alternative origins document naming `suiteFrontend`, padded with whitespace
   *  to exactly `size` bytes. */
  const documentOfSize = (size: number): string => {
    const document = JSON.stringify({ alternativeOrigins: [suiteFrontend] });
    return document.padEnd(size, " ");
  };

  /** Answers every fetch with `body`, recording the URLs asked for. */
  const serve = (body: string): string[] => {
    const fetchedUrls: string[] = [];
    vi.stubGlobal("fetch", (url: string) => {
      fetchedUrls.push(url);
      return Promise.resolve(
        new Response(body, {
          status: 200,
          headers: { "content-type": "application/json" },
        }),
      );
    });
    return fetchedUrls;
  };

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("reads a canister's list from its certified origin on icp.net", async () => {
    for (const origin of [
      `https://${canister}.ic0.app`,
      `https://${canister}.raw.ic0.app`,
    ]) {
      const fetchedUrls = serve(documentOfSize(0));

      await expect(fetchAlternativeOrigins(origin)).resolves.toEqual([
        suiteFrontend,
      ]);
      expect(fetchedUrls).toEqual([
        `https://${canister}.icp.net/.well-known/ii-alternative-origins`,
      ]);
    }
  });

  it("reads a list of up to 40 KiB, and none past it", async () => {
    serve(documentOfSize(40 * 1024));
    await expect(fetchAlternativeOrigins(APP)).resolves.toEqual([
      suiteFrontend,
    ]);

    serve(documentOfSize(40 * 1024 + 1));
    await expect(fetchAlternativeOrigins(APP)).resolves.toEqual([]);
  });

  it("opens the link of a frontend a list over 8 KiB names", async () => {
    serve(documentOfSize(20 * 1024));
    const origin = `https://${canister}.ic0.app`;

    const alternativeOrigins = await fetchAlternativeOrigins(origin);

    expect(
      allowedLink({
        url: `${suiteFrontend}/inbox/7`,
        origin,
        alternativeOrigins,
      }),
    ).toBe(`${suiteFrontend}/inbox/7`);
  });
});
