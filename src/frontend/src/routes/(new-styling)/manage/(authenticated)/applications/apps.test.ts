import { describe, expect, it } from "vitest";
import type { ApplicationInfo } from "$lib/generated/internet_identity_types";
import { appUrl, appsFrom } from "./apps";

const millis = (value: number): bigint => BigInt(value) * BigInt(1_000_000);

const application = (
  origin: string,
  last_used: bigint,
  notifications_allowed = false,
  last_notified?: bigint,
): ApplicationInfo => ({
  origin,
  last_used,
  notifications_allowed,
  last_notified: last_notified !== undefined ? [last_notified] : [],
});

describe("appsFrom", () => {
  it("lists nothing for an identity with no applications", () => {
    expect(appsFrom([])).toEqual([]);
  });

  it("lists the most recently used app first", () => {
    expect(
      appsFrom([
        application("https://older.example", millis(1_000)),
        application(
          "https://newer.example",
          millis(5_000),
          true,
          millis(3_000),
        ),
      ]),
    ).toEqual([
      {
        origin: "https://newer.example",
        lastUsedMillis: 5_000,
        notificationsAllowed: true,
        lastNotifiedMillis: 3_000,
      },
      {
        origin: "https://older.example",
        lastUsedMillis: 1_000,
        notificationsAllowed: false,
        lastNotifiedMillis: undefined,
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

describe("appUrl", () => {
  it("moves a canister on any gateway domain to icp.net", () => {
    for (const origin of [
      "https://2vxsx-fae.ic0.app",
      "https://2vxsx-fae.icp0.io",
      "https://2vxsx-fae.icp.net",
    ]) {
      expect(appUrl(origin)).toBe("https://2vxsx-fae.icp.net");
    }
  });

  it("keeps the raw label", () => {
    expect(appUrl("https://2vxsx-fae.raw.ic0.app")).toBe(
      "https://2vxsx-fae.raw.icp.net",
    );
  });

  it("leaves a custom domain alone", () => {
    expect(appUrl("https://oisy.com")).toBe("https://oisy.com");
  });
});
