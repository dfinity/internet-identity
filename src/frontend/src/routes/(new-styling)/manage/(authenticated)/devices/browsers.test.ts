import { describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  HIDDEN_AFTER_DAYS,
  SIGNED_OUT_AFTER_DAYS,
  brandNameOf,
  fromCanisterBrowsers,
  groupBrowsers,
  lastUsedAgeMillis,
  isSignedOut,
  kindOf,
  nameOf,
  platformWordOf,
  signOutBrowser,
} from "./browsers";
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

describe("kindOf", () => {
  const withFormFactor = (form_factor: BrowserDescription["form_factor"]) => ({
    ...CHROME_ON_A_MAC,
    form_factor,
  });

  it.each([
    [withFormFactor({ Mobile: null }), "phone"],
    [withFormFactor({ Tablet: null }), "tablet"],
    [withFormFactor({ Desktop: null }), "laptop"],
    [withFormFactor({ Unknown: null }), "unknown"],
  ])("draws %o as a %s", (description, kind) => {
    expect(kindOf(description)).toBe(kind);
  });
});

describe("brandNameOf and platformWordOf", () => {
  /// Pinned, because these strings are what a user reads to recognise their own browser:
  /// a token resolving to a different word is a row they no longer know themselves by.
  /// The row carries the brand, the group heading the platform.
  it.each([
    [describing({ Chrome: null }, { Android: null }), "Chrome", "Android"],
    [describing({ Safari: null }, { Ios: null }), "Safari", "iPhone"],
    [describing({ Safari: null }, { Ipados: null }), "Safari", "iPad"],
    [describing({ Safari: null }, { Macos: null }), "Safari", "Mac"],
    [describing({ Edge: null }, { Windows: null }), "Edge", "Windows"],
    [describing({ Chrome: null }, { ChromeOs: null }), "Chrome", "Chromebook"],
    [
      describing({ SamsungInternet: null }, { Android: null }),
      "Samsung Internet",
      "Android",
    ],
  ])("splits %o into %s and %s", (description, brand, platform) => {
    expect(brandNameOf(description)).toBe(brand);
    expect(platformWordOf(description)).toBe(platform);
  });

  /// A browser or system this frontend does not name is the row the list exists for, so
  /// it shows the token that arrived rather than a generic word.
  it("shows an unrecognised token as it arrived", () => {
    const unknown = describing({ Other: "YaBrowser" }, { Other: "HarmonyOS" });
    expect(brandNameOf(unknown)).toBe("YaBrowser");
    expect(platformWordOf(unknown)).toBe("HarmonyOS");
  });

  /// The model names the machine in prose, but never the group: two machines of one make
  /// report the same thing, which is why the heading hedges with "device(s)" instead.
  it("keeps the model out of the platform word", () => {
    const pixel = describing({ Chrome: null }, { Android: null }, ["Pixel 9"]);
    expect(platformWordOf(pixel)).toBe("Android");
    expect(nameOf(pixel)).toBe("Chrome on Pixel 9");
  });
});

describe("isSignedOut", () => {
  const DAY = 86_400_000;
  const now = Date.UTC(2026, 0, 31);
  const idle = (daysAgo: number, sessionCount: number) =>
    fromCanisterBrowsers([
      [
        {
          ...browser(1, BigInt(now - daysAgo * DAY) * BigInt(1_000_000)),
          session_count: sessionCount,
        },
      ],
    ])[0];

  it("holds a browser with a live session signed in", () => {
    expect(isSignedOut(idle(SIGNED_OUT_AFTER_DAYS - 1, 2), now)).toBe(false);
  });

  it("counts a browser with no stored session as signed out", () => {
    expect(isSignedOut(idle(0, 0), now)).toBe(true);
  });

  /// Every session such a browser opened is past MAX_SESSION_TTL_NS, so a count above
  /// zero here only means no write has pruned them yet.
  it("counts a browser idle past the window as signed out whatever the count says", () => {
    expect(isSignedOut(idle(SIGNED_OUT_AFTER_DAYS, 5), now)).toBe(true);
  });
});

describe("lastUsedAgeMillis", () => {
  const MINUTE = 60_000;
  const now = Date.UTC(2026, 0, 31, 12);
  const aged = (minutesAgo: number) =>
    fromCanisterBrowsers([
      [browser(1, BigInt(now - minutesAgo * MINUTE) * BigInt(1_000_000))],
    ])[0];

  /// The record cannot tell a browser still in use from one idle four minutes, so the
  /// first grain reports no figure and the page says what both have in common.
  it.each([0, 1, 4, 4.99])("reports no age %s minutes in", (minutesAgo) => {
    expect(lastUsedAgeMillis(aged(minutesAgo), now)).toBeUndefined();
  });

  /// Rounded up so the figure never claims more recency than the stamp can support,
  /// and counted from the end of the first grain so one grain is the smallest figure
  /// shown: measuring from zero would round anything past the threshold to two and
  /// leave "5 minutes ago" unreachable.
  it.each([
    [5, 5],
    [5.1, 5],
    [9, 5],
    [10, 5],
    [10.1, 10],
    [11, 10],
    [15, 10],
    [16, 15],
    [61, 60],
  ])("rounds %s minutes up to %s", (minutesAgo, expected) => {
    expect(lastUsedAgeMillis(aged(minutesAgo), now)).toBe(expected * MINUTE);
  });

  /// A stamp in the future is the clock disagreeing, not a browser used later: it falls
  /// in the first grain like anything else too recent to measure.
  it("reports no age for a stamp ahead of now", () => {
    expect(lastUsedAgeMillis(aged(-1), now)).toBeUndefined();
  });
});

describe("groupBrowsers", () => {
  const DAY = 86_400_000;
  const now = Date.UTC(2026, 0, 31);
  const at = (
    id: number,
    daysAgo: number,
    description: BrowserDescription,
  ) => ({
    ...browser(id, BigInt(now - daysAgo * DAY) * BigInt(1_000_000)),
    description,
    session_count: 1,
  });
  const CHROME_ON_WINDOWS = describing({ Chrome: null }, { Windows: null });
  const SAFARI_ON_IPHONE = describing({ Safari: null }, { Ios: null });

  it("groups browsers by the platform they run on", () => {
    const groups = groupBrowsers(
      fromCanisterBrowsers([
        [
          at(1, 5, CHROME_ON_A_MAC),
          at(2, 1, CHROME_ON_WINDOWS),
          at(3, 3, describing({ Safari: null }, { Macos: null })),
        ],
      ]),
      now,
    );

    expect(
      groups.map((group) => [
        group.platform,
        group.browsers.map((entry) => entry.id),
      ]),
    ).toEqual([
      ["Windows", [2]],
      ["Mac", [3, 1]],
    ]);
  });

  /// The group the user is looking at comes first, and so does the row they are on.
  it("orders groups and rows by most recent use", () => {
    const groups = groupBrowsers(
      fromCanisterBrowsers([
        [at(1, 10, CHROME_ON_A_MAC), at(2, 2, SAFARI_ON_IPHONE)],
      ]),
      now,
    );

    expect(groups.map((group) => group.platform)).toEqual(["iPhone", "Mac"]);
  });

  /// The one row the user can place without reading it. The canister returns records in
  /// registration order, which says nothing about which browser is asking.
  it("leads the group with the browser being read from", () => {
    const [group] = groupBrowsers(
      fromCanisterBrowsers(
        [[at(1, 5, CHROME_ON_A_MAC), at(2, 1, CHROME_ON_A_MAC)]],
        1,
      ),
      now,
    );

    expect(group.browsers.map((entry) => entry.id)).toEqual([1, 2]);
  });

  it("heads the group with the glyph of its platform", () => {
    const [group] = groupBrowsers(
      fromCanisterBrowsers([
        [
          {
            ...at(1, 1, SAFARI_ON_IPHONE),
            description: { ...SAFARI_ON_IPHONE, form_factor: { Mobile: null } },
          },
        ],
      ]),
      now,
    );

    expect(group.kind).toBe("phone");
  });

  /// Nothing can be signed out on a browser this idle, and keeping it buries the rows
  /// that can.
  it("leaves out a browser idle past the hiding window", () => {
    const groups = groupBrowsers(
      fromCanisterBrowsers([
        [at(1, HIDDEN_AFTER_DAYS, CHROME_ON_A_MAC), at(2, 1, SAFARI_ON_IPHONE)],
      ]),
      now,
    );

    expect(groups.map((group) => group.platform)).toEqual(["iPhone"]);
  });

  it("keeps a browser one day short of it", () => {
    const groups = groupBrowsers(
      fromCanisterBrowsers([[at(1, HIDDEN_AFTER_DAYS - 1, CHROME_ON_A_MAC)]]),
      now,
    );

    expect(groups.map((group) => group.platform)).toEqual(["Mac"]);
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
        description: CHROME_ON_A_MAC,
        createdAtMillis: 1_500,
        lastUsedMillis: 4_200,
        sessionCount: 0,
        isCurrent: false,
      },
    ]);
  });

  it("carries the session count, which is what tells a signed-out browser apart", () => {
    const [entry] = fromCanisterBrowsers([
      [{ ...browser(1, BigInt(1_000_000_000)), session_count: 3 }],
    ]);

    expect(entry.sessionCount).toBe(3);
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
