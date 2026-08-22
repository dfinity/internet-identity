import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import {
  appSessionFor,
  appSessionsForOrigin,
  discardAppSession,
  purgeAppSessions,
  storeAppSession,
  type AppSessionRecord,
} from "./app-session.store";

const ORIGIN = "https://app.example.com";

const record = (expiresAtMillis: number): AppSessionRecord => ({
  keyPair: {} as CryptoKeyPair,
  chainJson: "{}",
  expiresAtMillis,
  createdAtNanos: BigInt(1_000),
  accessLevel: "full-access" as const,
  accountPrincipal: "2vxsx-fae",
});

const anHourFromNow = () => Date.now() + 60 * 60 * 1000;

describe("app session store", () => {
  beforeEach(async () => {
    await purgeAppSessions(BigInt(10_000));
    await purgeAppSessions(BigInt(10_001));
    vi.useRealTimers();
  });

  it("returns a stored session for the same identity, account and origin", async () => {
    const key = { identityNumber: BigInt(10_000), origin: ORIGIN };
    await storeAppSession(key, record(anHourFromNow()));

    await expect(appSessionFor(key)).resolves.toMatchObject({
      accountPrincipal: "2vxsx-fae",
    });
  });

  it("keeps accounts of one identity apart", async () => {
    const identityNumber = BigInt(10_000);
    await storeAppSession(
      { identityNumber, origin: ORIGIN },
      record(anHourFromNow()),
    );

    await expect(
      appSessionFor({
        identityNumber,
        accountNumber: BigInt(3),
        origin: ORIGIN,
      }),
    ).resolves.toBeUndefined();
  });

  it("does not serve a session that is about to expire", async () => {
    const key = { identityNumber: BigInt(10_000), origin: ORIGIN };
    await storeAppSession(key, record(Date.now() + 60 * 1000));

    await expect(appSessionFor(key)).resolves.toBeUndefined();
  });

  it("discards a session", async () => {
    const key = { identityNumber: BigInt(10_000), origin: ORIGIN };
    await storeAppSession(key, record(anHourFromNow()));

    await discardAppSession(key);

    await expect(appSessionFor(key)).resolves.toBeUndefined();
  });

  it("lists every identity holding a session at one origin", async () => {
    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: ORIGIN },
      record(anHourFromNow()),
    );
    await storeAppSession(
      {
        identityNumber: BigInt(10_001),
        accountNumber: BigInt(7),
        origin: ORIGIN,
      },
      record(anHourFromNow()),
    );
    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: "https://other.example.com" },
      record(anHourFromNow()),
    );

    const held = await appSessionsForOrigin(ORIGIN);

    expect(held).toHaveLength(2);
    expect(held.map((entry) => entry.identityNumber).sort()).toEqual([
      BigInt(10_000),
      BigInt(10_001),
    ]);
    expect(
      held.find((entry) => entry.identityNumber === BigInt(10_001))
        ?.accountNumber,
    ).toBe(BigInt(7));
  });

  it("omits expiring sessions from the origin listing", async () => {
    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: ORIGIN },
      record(Date.now() + 60 * 1000),
    );

    await expect(appSessionsForOrigin(ORIGIN)).resolves.toEqual([]);
  });

  it("purges every session of one identity", async () => {
    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: ORIGIN },
      record(anHourFromNow()),
    );
    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: "https://other.example.com" },
      record(anHourFromNow()),
    );
    await storeAppSession(
      { identityNumber: BigInt(10_001), origin: ORIGIN },
      record(anHourFromNow()),
    );

    await purgeAppSessions(BigInt(10_000));

    await expect(appSessionsForOrigin(ORIGIN)).resolves.toHaveLength(1);
    await expect(
      appSessionsForOrigin("https://other.example.com"),
    ).resolves.toEqual([]);
  });
});
