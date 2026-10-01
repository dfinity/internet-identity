import { describe, expect, it } from "vitest";
import { appsFrom } from "./apps";

describe("appsFrom", () => {
  it("lists nothing for an identity with no recorded sign-ins", () => {
    expect(appsFrom(undefined)).toEqual([]);
    expect(appsFrom({})).toEqual([]);
  });

  it("lists the most recently visited app first", () => {
    expect(
      appsFrom({
        "https://older.example": {
          displayOrigin: "https://older.example",
          lastVisitedMillis: 1_000,
        },
        "https://newer.example": {
          displayOrigin: "https://newer.example",
          lastVisitedMillis: 5_000,
        },
      }),
    ).toEqual([
      {
        origin: "https://newer.example",
        displayOrigin: "https://newer.example",
        lastVisitedMillis: 5_000,
      },
      {
        origin: "https://older.example",
        displayOrigin: "https://older.example",
        lastVisitedMillis: 1_000,
      },
    ]);
  });

  it("orders apps visited at the same time by origin, so the list holds still", () => {
    const at = (origin: string) => ({
      displayOrigin: origin,
      lastVisitedMillis: 1_000,
    });

    expect(
      appsFrom({
        "https://b.example": at("https://b.example"),
        "https://a.example": at("https://a.example"),
      }).map(({ origin }) => origin),
    ).toEqual(["https://a.example", "https://b.example"]);
  });

  // An app deriving its identity from another origin is known by the one it is used on.
  it("keeps the origin the user signed in from apart from the derivation origin", () => {
    expect(
      appsFrom({
        "https://auth.app.example": {
          displayOrigin: "https://www.app.example",
          lastVisitedMillis: 1_000,
        },
      }),
    ).toEqual([
      {
        origin: "https://auth.app.example",
        displayOrigin: "https://www.app.example",
        lastVisitedMillis: 1_000,
      },
    ]);
  });

  it.each(["javascript:alert(1)", "https://app.example/path", "not a url"])(
    "falls back to the derivation origin for a display origin of %s",
    (displayOrigin) => {
      expect(
        appsFrom({
          "https://app.example": { displayOrigin, lastVisitedMillis: 1_000 },
        })[0].displayOrigin,
      ).toEqual("https://app.example");
    },
  );
});
