import { beforeEach, describe, expect, it, vi } from "vitest";
vi.mock("$lib/stores/browser-key.store", () => ({
  registeredIdentityNumbers: vi.fn(),
  signNotificationAppClaim: vi.fn(),
}));
vi.mock("./browserActor", () => ({ browserKeyActor: vi.fn() }));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  registeredIdentityNumbers,
  signNotificationAppClaim,
} from "$lib/stores/browser-key.store";
import { browserKeyActor } from "./browserActor";
import { claimDeviceApp, CLAIM_TOKEN_LIFETIME_MS } from "./deviceApp";

const IDENTITY = BigInt(10_000);
const DONOR = BigInt(20_000);
const OTHER_DONOR = BigInt(30_000);
const SIGNATURE = new Uint8Array([1, 2, 3]);
const ok = { Ok: null } as const;

const identities = vi.mocked(registeredIdentityNumbers);
const sign = vi.mocked(signNotificationAppClaim);
const actorFor = vi.mocked(browserKeyActor);

/** What the one call a claim makes carries, so the assertions read its fields. */
interface ClaimRequest {
  anchor_number: bigint;
  offering_anchor_number: bigint;
  expires_at_ns: bigint;
  signature: Uint8Array;
}

/** The claim call, as the browser holding the claiming identity's key. */
const browser = (claim: () => Promise<unknown>) => {
  const claim_notification_app = vi.fn((_request: ClaimRequest) => claim());
  actorFor.mockImplementation(() =>
    Promise.resolve({
      claim_notification_app,
    } as unknown as ActorSubclass<_SERVICE>),
  );
  return claim_notification_app;
};

/** Which identity each call offered the app, in the order they were asked. */
const offeringAnchors = (claim: ReturnType<typeof browser>): bigint[] =>
  claim.mock.calls.map(([request]) => request.offering_anchor_number);

describe("claimDeviceApp", () => {
  let logged: string[];

  beforeEach(() => {
    vi.clearAllMocks();
    logged = [];
    const record = (...args: unknown[]) => {
      logged.push(args.map(String).join(" "));
    };
    vi.spyOn(console, "warn").mockImplementation(record);
    vi.spyOn(console, "error").mockImplementation(record);
    identities.mockResolvedValue([DONOR, IDENTITY]);
    sign.mockResolvedValue(SIGNATURE);
  });

  it("claims the app of the other identity signed in here", async () => {
    const claim = browser(() => Promise.resolve(ok));

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("claimed");
    expect(claim).toHaveBeenCalledTimes(1);
    expect(claim).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      offering_anchor_number: DONOR,
      expires_at_ns: expect.any(BigInt),
      signature: SIGNATURE,
    });
  });

  it("signs the token as the identity that has the app", async () => {
    browser(() => Promise.resolve(ok));

    await claimDeviceApp(IDENTITY);

    expect(sign).toHaveBeenCalledWith({
      offeringIdentityNumber: DONOR,
      claimingIdentityNumber: IDENTITY,
      expiresAtNs: expect.any(BigInt),
    });
  });

  /// The clock is held still: reading it before the call and comparing against a window
  /// the call reads it again for makes the bound depend on a tick landing mid-await.
  it("bounds the expiry it signs to the window it asks for", async () => {
    vi.useFakeTimers();
    try {
      const claim = browser(() => Promise.resolve(ok));
      const nowNs = BigInt(Date.now()) * BigInt(1_000_000);

      await claimDeviceApp(IDENTITY);

      const [{ expires_at_ns: expiresAtNs }] = claim.mock.calls[0];
      expect(expiresAtNs).toBe(
        nowNs + BigInt(CLAIM_TOKEN_LIFETIME_MS) * BigInt(1_000_000),
      );
    } finally {
      vi.useRealTimers();
    }
  });

  /// The canister reads its own clock, so asking for its whole window would refuse every
  /// phone running ahead of it. The headroom is the point, not an accident.
  it("asks for well under the window the canister allows", () => {
    expect(CLAIM_TOKEN_LIFETIME_MS).toBeLessThan(30 * 60 * 1000);
  });

  it("asks the next identity where one refuses, and none of them twice", async () => {
    identities.mockResolvedValue([DONOR, OTHER_DONOR, IDENTITY]);
    const claim = browser(() =>
      claim.mock.calls.length === 1
        ? Promise.resolve({ Err: { NoNotificationApp: null } })
        : Promise.resolve(ok),
    );

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("claimed");
    expect(offeringAnchors(claim)).toEqual([DONOR, OTHER_DONOR]);
  });

  it("gives up once every identity here has refused", async () => {
    identities.mockResolvedValue([DONOR, OTHER_DONOR, IDENTITY]);
    const claim = browser(() =>
      Promise.resolve({ Err: { NoNotificationApp: null } }),
    );

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("no-donor");
    expect(offeringAnchors(claim)).toEqual([DONOR, OTHER_DONOR]);
  });

  it("asks no identity but the others signed in here", async () => {
    identities.mockResolvedValue([IDENTITY]);
    const claim = browser(() => Promise.resolve(ok));

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("no-donor");
    expect(claim).not.toHaveBeenCalled();
    expect(sign).not.toHaveBeenCalled();
  });

  it("answers nothing learned where the key store would not open", async () => {
    identities.mockRejectedValue(new Error("no storage"));
    const claim = browser(() => Promise.resolve(ok));

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("indeterminate");
    expect(claim).not.toHaveBeenCalled();
    expect(logged.join("\n")).toContain("no storage");
  });

  it("passes over an identity this browser is not registered for", async () => {
    identities.mockResolvedValue([DONOR, OTHER_DONOR, IDENTITY]);
    sign.mockImplementation(({ offeringIdentityNumber }) =>
      Promise.resolve(offeringIdentityNumber === DONOR ? undefined : SIGNATURE),
    );
    const claim = browser(() => Promise.resolve(ok));

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("claimed");
    expect(offeringAnchors(claim)).toEqual([OTHER_DONOR]);
    expect(logged.join("\n")).toContain(`anchor ${DONOR}`);
  });

  it("names the refusal the canister sent", async () => {
    browser(() =>
      Promise.resolve({ Err: { InternalCanisterError: "the record is gone" } }),
    );

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("no-donor");
    expect(logged.join("\n")).toContain("InternalCanisterError");
    expect(logged.join("\n")).toContain("the record is gone");
  });

  it("names a call that never reached the canister", async () => {
    browser(() => Promise.reject(new Error("the gateway refused")));

    await expect(claimDeviceApp(IDENTITY)).resolves.toBe("no-donor");
    expect(logged.join("\n")).toContain("the gateway refused");
  });
});
