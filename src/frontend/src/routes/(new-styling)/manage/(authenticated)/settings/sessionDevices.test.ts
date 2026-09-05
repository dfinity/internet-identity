import { describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  fromCanisterSessionDevices,
  signOutSessionDevice,
} from "./sessionDevices";

const device = (
  id: number,
  name: string,
  createdAtNanos: bigint,
  lastUsedNanos: bigint = createdAtNanos,
) => ({
  id,
  name,
  created_at: createdAtNanos,
  last_used: lastUsedNanos,
});

describe("fromCanisterSessionDevices", () => {
  it("reports no devices for an identity that has never created a session", () => {
    expect(fromCanisterSessionDevices([])).toEqual([]);
  });

  it("shows the most recently used browser first", () => {
    expect(
      fromCanisterSessionDevices([
        [
          device(1, "Firefox on Linux", BigInt(1_000_000_000)),
          device(2, "Chrome on macOS", BigInt(3_000_000_000)),
          device(3, "Safari on iOS", BigInt(2_000_000_000)),
        ],
      ]).map((entry) => entry.name),
    ).toEqual(["Chrome on macOS", "Safari on iOS", "Firefox on Linux"]);
  });

  it("orders on use rather than on registration", () => {
    expect(
      fromCanisterSessionDevices([
        [
          device(
            1,
            "enrolled first, still in use",
            BigInt(1),
            BigInt(9_000_000_000),
          ),
          device(2, "enrolled later, gone quiet", BigInt(5_000_000_000)),
        ],
      ]).map((entry) => entry.name),
    ).toEqual(["enrolled first, still in use", "enrolled later, gone quiet"]);
  });

  it("converts both timestamps to milliseconds", () => {
    expect(
      fromCanisterSessionDevices([
        [device(1, "Chrome", BigInt(1_500_000_000), BigInt(4_200_000_000))],
      ]),
    ).toEqual([
      {
        id: 1,
        name: "Chrome",
        createdAtMillis: 1_500,
        lastUsedMillis: 4_200,
        isCurrent: false,
      },
    ]);
  });

  it("marks the browser being read from, so two of one name can be told apart", () => {
    const marked = fromCanisterSessionDevices(
      [
        [
          device(1, "Chrome on Mac", BigInt(1_000_000_000)),
          device(2, "Chrome on Mac", BigInt(2_000_000_000)),
        ],
      ],
      2,
    );

    expect(marked.map((entry) => [entry.id, entry.isCurrent])).toEqual([
      [2, true],
      [1, false],
    ]);
  });

  it("marks nothing when this browser has never created a session", () => {
    expect(
      fromCanisterSessionDevices([
        [device(1, "Chrome on Mac", BigInt(1_000_000_000))],
      ]).some((entry) => entry.isCurrent),
    ).toBe(false);
  });

  /// An id from another browser's record must not mark an entry here.
  it("marks nothing when the id is one this identity does not hold", () => {
    expect(
      fromCanisterSessionDevices(
        [[device(1, "Chrome on Mac", BigInt(1_000_000_000))]],
        99,
      ).some((entry) => entry.isCurrent),
    ).toBe(false);
  });
});

describe("signOutSessionDevice", () => {
  it("names the browser by id and nothing else", async () => {
    const revoke_device_sessions = vi.fn(() => Promise.resolve({ Ok: null }));
    const actor = {
      revoke_device_sessions,
    } as unknown as ActorSubclass<_SERVICE>;

    await signOutSessionDevice(actor, BigInt(10_000), 3);

    expect(revoke_device_sessions).toHaveBeenCalledWith({
      identity_number: BigInt(10_000),
      device_id: 3,
    });
  });

  it("surfaces an unauthorized refusal", async () => {
    const actor = {
      revoke_device_sessions: () =>
        Promise.resolve({ Err: { Unauthorized: "2vxsx-fae" } }),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(
      signOutSessionDevice(actor, BigInt(10_000), 3),
    ).rejects.toThrow(/Not authorized/);
  });

  it("surfaces an internal failure", async () => {
    const actor = {
      revoke_device_sessions: () =>
        Promise.resolve({ Err: { InternalCanisterError: "boom" } }),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(
      signOutSessionDevice(actor, BigInt(10_000), 3),
    ).rejects.toThrow("boom");
  });

  /// Which browser is signing out is read from the key record, not passed in: the list
  /// renders that flag from a promise, and a click landing before it resolved used to
  /// leave this browser's own chains behind — the one thing signing out must not do.
  it("discards this browser's stored chains, and another browser's not", async () => {
    const { storeAppSession, appSessionsForOrigin } =
      await import("$lib/stores/app-session.store");
    const { set: idbSet, createStore } = await import("idb-keyval");
    const record = {
      keyPair: undefined as unknown as CryptoKeyPair,
      chainJson: "{}",
      expiresAtMillis: Date.now() + 60 * 60 * 1000,
      sessionId: BigInt(1_000),
      accessLevel: "full-access" as const,
      accountPrincipal: "2vxsx-fae",
    };
    const actor = {
      revoke_device_sessions: vi.fn(() => Promise.resolve({ Ok: null })),
    } as unknown as ActorSubclass<_SERVICE>;
    // This browser is device 3.
    await idbSet(
      BigInt(10_000).toString(),
      { keyPair: undefined, deviceId: 3 },
      createStore("ii-browser-keys", "keys"),
    );

    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: "https://app.example.com" },
      record,
    );
    // Signing another browser out must leave this one signed in locally.
    await signOutSessionDevice(actor, BigInt(10_000), 9);
    expect(await appSessionsForOrigin("https://app.example.com")).toHaveLength(
      1,
    );

    await signOutSessionDevice(actor, BigInt(10_000), 3);
    expect(await appSessionsForOrigin("https://app.example.com")).toEqual([]);
  });
});
