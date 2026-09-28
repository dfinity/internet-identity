import { describe, expect, it } from "vitest";
import {
  allowedLink,
  parseAlternativeOrigins,
} from "$lib/utils/notifications/notificationLink";

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
    ).toBe("https://vt36r-2qaaa-aaaad-aad5a-cai.ic0.app");
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
