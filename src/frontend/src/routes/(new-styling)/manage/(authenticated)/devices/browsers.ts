import type { ActorSubclass } from "@icp-sdk/core/agent";
import { purgeAppSessions } from "$lib/stores/app-session.store";
import { currentBrowserId } from "$lib/stores/browser-key.store";
import type {
  _SERVICE,
  BrowserBrand,
  BrowserDescription,
  BrowserInfo,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";

export interface Browser {
  id: number;
  /** Derived here rather than stored, so renaming a product renames every row at once. */
  name: string;
  description: BrowserDescription;
  createdAtMillis: number;
  lastUsedMillis: number;
  /** Counts stored records, so a browser long gone can still read above zero until some
   *  write prunes its expired sessions. [`isSignedOut`] is what the page asks instead. */
  sessionCount: number;
  /** Several browsers report the same name, so the list marks the one being read from. */
  isCurrent: boolean;
}

/** Which glyph stands for the machine. Four, because that is what the data can tell. */
export type DeviceKind = "laptop" | "phone" | "tablet" | "unknown";

/**
 * `Desktop` draws a laptop: nothing reported separates a laptop from a tower, and of the
 * two a laptop is the likelier machine to be reading this page.
 */
export const kindOf = (description: BrowserDescription): DeviceKind =>
  "Mobile" in description.form_factor
    ? "phone"
    : "Tablet" in description.form_factor
      ? "tablet"
      : "Desktop" in description.form_factor
        ? "laptop"
        : "unknown";

const BRAND_ICONS = import.meta.glob("./icons/*.svg", {
  eager: true,
  query: "?url",
  import: "default",
}) as Record<string, string>;

const iconFor = (file: string): string | undefined =>
  Object.entries(BRAND_ICONS).find(([path]) => path.endsWith(`/${file}`))?.[1];

const BRAND_ICON_FILES: Record<string, string> = {
  Chrome: "chrome.svg",
  Safari: "safari.svg",
  Firefox: "firefox.svg",
  Edge: "edge.svg",
  Opera: "opera.svg",
  SamsungInternet: "samsung-internet.svg",
};

/**
 * The brand mark for the badge, where the brand is one of the six that has one.
 *
 * A browser resolved to `Other` has a name but no icon, which is the point of the bar for
 * a named variant: an icon nobody recognises says less than the name written out.
 */
export const brandIconOf = (
  description: BrowserDescription,
): string | undefined => {
  const [tag] = Object.entries(description.brand)[0];
  const file = BRAND_ICON_FILES[tag];
  return file === undefined ? undefined : iconFor(file);
};

/**
 * The id of the row for a browser the canister holds no record of — the one being read
 * from, before it has signed in to an app. Outside `nat32`, so it cannot collide with a
 * registered browser's id.
 */
export const NO_RECORD_ID = -1;

const DAY_MILLIS = 86_400_000;
const MINUTE_MILLIS = 60_000;

/**
 * How coarse `last_used` really is.
 *
 * The canister stamps it with the exact time, but only when something happens, and in
 * steady use that is the app minting its next app delegation — which the canister gives
 * a five minute life. So a browser in active use carries a stamp up to five minutes
 * behind, and a figure read off it to the minute claims a freshness the record does not
 * have.
 */
export const LAST_USED_GRAIN_MILLIS = 5 * MINUTE_MILLIS;

/**
 * The age to show for a browser, rounded up to the grain, or `undefined` for one used
 * within a single grain.
 *
 * Up rather than down: the stamp is already behind by up to one grain, so rounding up
 * lands inside the interval the true figure is in, and the page never says a browser
 * was used more recently than it was. `undefined` is the first interval, where the
 * record cannot tell a browser still in use from one idle four minutes — so the page
 * says what both have in common instead of picking a number.
 */
export const lastUsedAgeMillis = (
  browser: Browser,
  now: number,
): number | undefined => {
  const age = now - browser.lastUsedMillis;
  if (age < LAST_USED_GRAIN_MILLIS) return undefined;
  return Math.ceil(age / LAST_USED_GRAIN_MILLIS) * LAST_USED_GRAIN_MILLIS;
};

/**
 * A browser idle this long holds nothing that can still be minted from: the canister's
 * `MAX_SESSION_TTL_NS` is 30 days, `last_used` advances on every refresh, so every
 * session such a browser opened is past its absolute bound.
 *
 * Asked instead of `sessionCount` alone, which counts stored records and can read above
 * zero until some write prunes the expired ones.
 */
export const SIGNED_OUT_AFTER_DAYS = 30;

/** Idle this long and the browser is not shown at all. */
export const HIDDEN_AFTER_DAYS = 90;

/** Whole days since a browser last did anything, rounded down. */
export const daysIdle = (browser: Browser, now: number): number =>
  Math.floor((now - browser.lastUsedMillis) / DAY_MILLIS);

/** Whether this browser can still reach an app, as far as the page can tell. */
export const isSignedOut = (browser: Browser, now: number): boolean =>
  browser.sessionCount === 0 || daysIdle(browser, now) >= SIGNED_OUT_AFTER_DAYS;

const BRAND_NAMES: Record<string, string> = {
  Chrome: "Chrome",
  Safari: "Safari",
  Firefox: "Firefox",
  Edge: "Edge",
  Opera: "Opera",
  SamsungInternet: "Samsung Internet",
};

/**
 * What the owner calls the thing, which is not the system's own name: nobody says they
 * are on "Chrome OS" or "iPadOS".
 */
const PLATFORM_WORDS: Record<string, string> = {
  Macos: "Mac",
  Ios: "iPhone",
  Ipados: "iPad",
  Windows: "Windows",
  Android: "Android",
  ChromeOs: "Chromebook",
  Linux: "Linux",
};

/** The token a variant carries where it names one, and its tag where it does not. */
const named = (
  variant: BrowserBrand | OperatingSystem,
  names: Record<string, string>,
): string => {
  const [tag, value] = Object.entries(variant)[0];
  return tag === "Other" ? String(value) : (names[tag] ?? tag);
};

/**
 * What a row is labelled: the brand alone, because the platform is on the heading of the
 * group the row sits in.
 *
 * An unrecognised token is shown as it arrived rather than as a generic word — a browser
 * the user does not recognise is the row this list exists for.
 */
export const brandNameOf = (description: BrowserDescription): string =>
  named(description.brand, BRAND_NAMES);

/**
 * What the group is keyed and headed by. The model is deliberately not part of it: two
 * machines of the same make report the same thing, which is why the heading says
 * "device(s)" rather than counting them.
 */
export const platformWordOf = (description: BrowserDescription): string =>
  named(description.os, PLATFORM_WORDS);

/**
 * How a browser is named in prose — the sign-out dialog and its toast — where it is the
 * only thing named and the brand alone would not say which machine.
 *
 * The hardware wins where the client could name it, because "Chrome on Pixel 9" is what
 * its owner recognises; the platform word stands in everywhere else.
 */
export const nameOf = (description: BrowserDescription): string =>
  `${brandNameOf(description)} on ${
    description.model[0] ?? platformWordOf(description)
  }`;

/** Browsers on one platform, in the order the page renders them. */
export interface BrowserGroup {
  /** The platform word, which is also what the group is keyed by. */
  platform: string;
  /** Which glyph heads the group, taken from its most recently used browser. */
  kind: DeviceKind;
  browsers: Browser[];
}

/**
 * Browsers grouped by the platform they run on, most recently used group first and most
 * recently used browser first within each.
 *
 * Browsers idle beyond [`HIDDEN_AFTER_DAYS`] are left out entirely: nothing can be
 * signed out there, and a list that keeps them buries the rows that matter.
 */
export const groupBrowsers = (
  browsers: Browser[],
  now: number,
): BrowserGroup[] => {
  const groups = new Map<string, BrowserGroup>();
  for (const browser of browsers) {
    if (daysIdle(browser, now) >= HIDDEN_AFTER_DAYS) {
      continue;
    }
    const platform = platformWordOf(browser.description);
    const group = groups.get(platform);
    if (group === undefined) {
      groups.set(platform, {
        platform,
        kind: kindOf(browser.description),
        browsers: [browser],
      });
    } else {
      group.browsers.push(browser);
    }
  }
  // The browser reading the page leads its group: it is the one row the user can place
  // without reading it, and the canister returns records in registration order, which
  // says nothing about that.
  for (const group of groups.values()) {
    group.browsers.sort(
      (one, other) => Number(other.isCurrent) - Number(one.isCurrent),
    );
  }
  return [...groups.values()];
};

export const fromCanisterBrowsers = (
  browsers: [] | [BrowserInfo[]],
  currentBrowserId?: number,
): Browser[] =>
  (browsers[0] ?? [])
    .map((browser) => ({
      id: browser.id,
      name: nameOf(browser.description),
      description: browser.description,
      createdAtMillis: nanosToMillis(browser.created_at),
      lastUsedMillis: nanosToMillis(browser.last_used),
      sessionCount: browser.session_count,
      isCurrent: browser.id === currentBrowserId,
    }))
    .sort((a, b) => b.lastUsedMillis - a.lastUsedMillis);

/**
 * Ends every session this browser holds, across every app it is signed into.
 *
 * The browser record itself survives, so a browser that has been signed out is still one
 * the user recognises and signing back in from it reuses the same entry.
 *
 * Signing *this* browser out also discards the session chains it holds locally. The
 * canister has already stopped honouring them, and leaving them would have the next
 * silent request offer a chain that cannot mint.
 *
 * Which browser this is comes from the key record here rather than from a caller: the
 * list renders that flag from a promise, so a click landing before it resolves would
 * otherwise pass `false` for the user's own browser and leave exactly those chains
 * behind.
 */
export const signOutBrowser = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  browserId: number,
): Promise<void> => {
  const result = await actor.revoke_browser_sessions({
    identity_number: identityNumber,
    browser_id: browserId,
  });
  if ("Err" in result) {
    throw new Error(
      "Unauthorized" in result.Err
        ? "Not authorized to end this browser's sessions"
        : result.Err.InternalCanisterError,
    );
  }
  if ((await currentBrowserId(identityNumber)) === browserId) {
    await purgeAppSessions(identityNumber);
  }
};
