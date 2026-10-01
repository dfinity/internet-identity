import { describe, expect, it } from "vitest";
import type { ApplicationInfo } from "$lib/generated/internet_identity_types";
import { appsFrom } from "./apps";

const millis = (value: number): bigint => BigInt(value) * BigInt(1_000_000);

const application = (
  origin: string,
  last_used: bigint,
  notifications_allowed = false,
): ApplicationInfo => ({ origin, last_used, notifications_allowed });

describe("appsFrom", () => {
  it("lists nothing for an identity with no applications", () => {
    expect(appsFrom([])).toEqual([]);
  });

  it("lists the most recently used app first", () => {
    expect(
      appsFrom([
        application("https://older.example", millis(1_000)),
        application("https://newer.example", millis(5_000), true),
      ]),
    ).toEqual([
      {
        origin: "https://newer.example",
        lastUsedMillis: 5_000,
        notificationsAllowed: true,
      },
      {
        origin: "https://older.example",
        lastUsedMillis: 1_000,
        notificationsAllowed: false,
      },
    ]);
  });

  it("orders apps used at the same time by origin, so the list holds still", () => {
    expect(
      appsFrom([
        application("https://b.example", millis(1_000)),
        application("https://a.example", millis(1_000)),
      ]).map(({ origin }) => origin),
    ).toEqual(["https://a.example", "https://b.example"]);
  });
});
