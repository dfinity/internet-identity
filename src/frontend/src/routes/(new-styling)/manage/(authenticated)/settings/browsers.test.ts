import { describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { fromCanisterBrowsers, nameOf, signOutBrowser } from "./browsers";
import type {
  BrowserBrand,
  BrowserDescription,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";

const describing = (
  brand: BrowserBrand,
  os: OperatingSystem,
  model: [] | [string] = [],
): BrowserDescription => ({ brand, os, form_factor: { Desktop: null }, model });

const CHROME_ON_A_MAC = describing({ Chrome: null }, { Macos: null });

const browser = (
  id: number,
  createdAtNanos: bigint,
  lastUsedNanos: bigint = createdAtNanos,
  description: BrowserDescription = CHROME_ON_A_MAC,
) => ({
  id,
  description,
  session_count: 0,
  created_at: createdAtNanos,
  last_used: lastUsedNanos,
});

describe("nameOf", () => {
  /// Pinned, because these strings are what a user reads to recognise their own browser:
  /// a token resolving to a different word is a row they no longer know themselves by.
  it.each([
    [describing({ Chrome: null }, { Android: null }), "Chrome on Android"],
    [describing({ Safari: null }, { Ios: null }), "Safari on iPhone"],
    [describing({ Safari: null }, { Ipados: null }), "Safari on iPad"],
    [describing({ Safari: null }, { Macos: null }), "Safari on Mac"],
    [describing({ Edge: null }, { Windows: null }), "Edge on Windows"],
    [describing({ Vivaldi: null }, { Linux: null }), "Vivaldi on Linux"],
    [describing({ Chrome: null }, { ChromeOs: null }), "Chrome on Chromebook"],
    [
      describing({ SamsungInternet: null }, { Android: null }),
      "Samsung Internet on Android",
    ],
    [describing({ Brave: null }, { Macos: null }), "Brave on Mac"],
  ])("names %o as %s", (description, expected) => {
    expect(nameOf(description)).toBe(expected);
  });

  /// What the owner recognises. The platform word only stands in where no model came.
  it("names the hardware where the client could name it", () => {
    expect(
      nameOf(describing({ Chrome: null }, { Android: null }, ["Pixel 9"])),
    ).toBe("Chrome on Pixel 9");
  });

  /// A browser or system this frontend does not name is the row the list exists for, so
  /// it shows the token that arrived rather than a generic word.
  it("shows an unrecognised token as it arrived", () => {
    expect(
      nameOf(describing({ Other: "YaBrowser" }, { Other: "HarmonyOS" })),
    ).toBe("YaBrowser on HarmonyOS");
  });
});

describe("fromCanisterBrowsers", () => {
  it("reports no browsers for an identity that has never created a session", () => {
    expect(fromCanisterBrowsers([])).toEqual([]);
  });

  it("shows the most recently used browser first", () => {
    expect(
      fromCanisterBrowsers([
        [
          browser(1, BigInt(1_000_000_000)),
          browser(2, BigInt(3_000_000_000)),
          browser(3, BigInt(2_000_000_000)),
        ],
      ]).map((entry) => entry.id),
    ).toEqual([2, 3, 1]);
  });

  it("orders on use rather than on registration", () => {
    expect(
      fromCanisterBrowsers([
        [
          // Registered first and still in use.
          browser(1, BigInt(1), BigInt(9_000_000_000)),
          // Registered later and gone quiet since.
          browser(2, BigInt(5_000_000_000)),
        ],
      ]).map((entry) => entry.id),
    ).toEqual([1, 2]);
  });

  it("converts both timestamps to milliseconds", () => {
    expect(
      fromCanisterBrowsers([
        [browser(1, BigInt(1_500_000_000), BigInt(4_200_000_000))],
      ]),
    ).toEqual([
      {
        id: 1,
        name: "Chrome on Mac",
        createdAtMillis: 1_500,
        lastUsedMillis: 4_200,
        isCurrent: false,
      },
    ]);
  });

  it("marks the browser being read from, so two of one name can be told apart", () => {
    const marked = fromCanisterBrowsers(
      [[browser(1, BigInt(1_000_000_000)), browser(2, BigInt(2_000_000_000))]],
      2,
    );

    expect(marked.map((entry) => [entry.id, entry.isCurrent])).toEqual([
      [2, true],
      [1, false],
    ]);
  });

  it("marks nothing when this browser has never created a session", () => {
    expect(
      fromCanisterBrowsers([[browser(1, BigInt(1_000_000_000))]]).some(
        (entry) => entry.isCurrent,
      ),
    ).toBe(false);
  });

  /// An id from another browser's record must not mark an entry here.
  it("marks nothing when the id is one this identity does not hold", () => {
    expect(
      fromCanisterBrowsers([[browser(1, BigInt(1_000_000_000))]], 99).some(
        (entry) => entry.isCurrent,
      ),
    ).toBe(false);
  });
});

describe("signOutBrowser", () => {
  it("names the browser by id and nothing else", async () => {
    const revoke_browser_sessions = vi.fn(() => Promise.resolve({ Ok: null }));
    const actor = {
      revoke_browser_sessions,
    } as unknown as ActorSubclass<_SERVICE>;

    await signOutBrowser(actor, BigInt(10_000), 3);

    expect(revoke_browser_sessions).toHaveBeenCalledWith({
      identity_number: BigInt(10_000),
      browser_id: 3,
    });
  });

  it("surfaces an internal failure", async () => {
    const actor = {
      revoke_browser_sessions: () =>
        Promise.resolve({ Err: { InternalCanisterError: "boom" } }),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(signOutBrowser(actor, BigInt(10_000), 3)).rejects.toThrow(
      "boom",
    );
  });

  it("surfaces an unauthorized refusal", async () => {
    const actor = {
      revoke_browser_sessions: () =>
        Promise.resolve({ Err: { Unauthorized: "2vxsx-fae" } }),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(signOutBrowser(actor, BigInt(10_000), 3)).rejects.toThrow(
      /Not authorized/,
    );
  });

  /// Which browser is signing out is read from the key record, not passed in: the list
  /// renders that flag from a promise, so a click landing before it resolves would pass
  /// `false` for the user's own browser and leave its chains behind — the one thing
  /// signing out must not do.
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
      revoke_browser_sessions: vi.fn(() => Promise.resolve({ Ok: null })),
    } as unknown as ActorSubclass<_SERVICE>;
    // This browser is browser 3.
    await idbSet(
      BigInt(10_000).toString(),
      { keyPair: undefined, browserId: 3 },
      createStore("ii-browser-keys", "keys"),
    );

    await storeAppSession(
      { identityNumber: BigInt(10_000), origin: "https://app.example.com" },
      record,
    );
    // Signing another browser out must leave this one signed in locally.
    await signOutBrowser(actor, BigInt(10_000), 9);
    expect(await appSessionsForOrigin("https://app.example.com")).toHaveLength(
      1,
    );

    await signOutBrowser(actor, BigInt(10_000), 3);
    expect(await appSessionsForOrigin("https://app.example.com")).toEqual([]);
  });
});
