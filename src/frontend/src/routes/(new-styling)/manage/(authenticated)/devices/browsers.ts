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
  /** Counts stored records, so a browser long gone still reads as signed in until some
   *  write prunes its expired sessions. That is what the inactive badge explains. */
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

/** A browser this identity has not been near in this long is worth a second look. */
export const INACTIVE_AFTER_DAYS = 30;

/**
 * Whole days since a browser last did anything, or `undefined` where that is not long
 * enough to say. `last_used` advances on every session refresh, so this measures the
 * browser rather than any one session.
 */
export const inactiveDays = (
  browser: Browser,
  now: number,
): number | undefined => {
  const days = Math.floor((now - browser.lastUsedMillis) / 86_400_000);
  return days >= INACTIVE_AFTER_DAYS ? days : undefined;
};

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
 * How a browser is named in the list.
 *
 * The hardware wins where the client could name it, because "Chrome on Pixel 9" is what
 * its owner recognises; the platform word stands in everywhere else. An unrecognised
 * token is shown as it arrived rather than as a generic fallback — a browser the user
 * does not recognise is the row this list exists for.
 */
export const nameOf = (description: BrowserDescription): string =>
  `${named(description.brand, BRAND_NAMES)} on ${
    description.model[0] ?? named(description.os, PLATFORM_WORDS)
  }`;

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
