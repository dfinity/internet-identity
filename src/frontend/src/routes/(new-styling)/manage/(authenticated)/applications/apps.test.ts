import { describe, expect, it } from "vitest";
import type {
  LastUsedAccount,
  LastUsedAccounts,
} from "$lib/stores/last-used-identities.store";
import { appsFrom } from "./apps";

const IDENTITY = BigInt(10_000);

const account = (
  origin: string,
  lastUsedTimestampMillis: number,
  accountNumber?: bigint,
): LastUsedAccount => ({
  identityNumber: IDENTITY,
  accountNumber,
  origin,
  lastUsedTimestampMillis,
});

describe("appsFrom", () => {
  it("lists nothing for an identity with no recorded sign-ins", () => {
    expect(appsFrom(undefined)).toEqual([]);
    expect(appsFrom({})).toEqual([]);
  });

  it("lists each app once, at its latest sign-in, most recent first", () => {
    const accounts: LastUsedAccounts = {
      "https://older.example": {
        primary: account("https://older.example", 1_000),
      },
      "https://newer.example": {
        primary: account("https://newer.example", 2_000),
        "7": account("https://newer.example", 5_000, BigInt(7)),
      },
    };

    expect(appsFrom(accounts)).toEqual([
      { origin: "https://newer.example", lastUsedMillis: 5_000 },
      { origin: "https://older.example", lastUsedMillis: 1_000 },
    ]);
  });

  // The canister reports an account never signed in to as 0, which is no time at all.
  it("treats a zero timestamp as unknown and lists the app last", () => {
    const accounts: LastUsedAccounts = {
      "https://unused.example": {
        primary: account("https://unused.example", 0),
      },
      "https://used.example": {
        primary: account("https://used.example", 1_000),
      },
    };

    expect(appsFrom(accounts)).toEqual([
      { origin: "https://used.example", lastUsedMillis: 1_000 },
      { origin: "https://unused.example", lastUsedMillis: undefined },
    ]);
  });

  it("orders apps with the same time by origin, so the list holds still", () => {
    const accounts: LastUsedAccounts = {
      "https://b.example": { primary: account("https://b.example", 0) },
      "https://a.example": { primary: account("https://a.example", 0) },
    };

    expect(appsFrom(accounts).map(({ origin }) => origin)).toEqual([
      "https://a.example",
      "https://b.example",
    ]);
  });

  it("skips an app whose accounts came back empty", () => {
    const accounts: LastUsedAccounts = {
      "https://empty.example": {},
      "https://used.example": {
        primary: account("https://used.example", 1_000),
      },
    };

    expect(appsFrom(accounts)).toEqual([
      { origin: "https://used.example", lastUsedMillis: 1_000 },
    ]);
  });
});
