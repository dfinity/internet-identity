import type {
  BrowserBrand,
  BrowserDescription,
  FormFactor,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";

/**
 * What this browser is, resolved into the tokens the canister stores.
 *
 * Resolved here rather than read back and parsed later: the canister keeps tokens and
 * never interprets them, so the wording a user reads lives entirely in this frontend and
 * a rename reaches every stored record at once.
 *
 * The user agent carries the brand and the system on every engine. Client hints are
 * Chromium-only — Safari and Firefox expose nothing — so they are used for the two things
 * an agent cannot give: the hardware model, and a form factor the browser states rather
 * than one inferred from its agent.
 */

/** The canister refuses a longer token, so a resolver never offers one. */
const MAX_BROWSER_TOKEN_BYTES = 64;

/** Ordered most specific first: every later token also appears in the earlier ones' agents. */
const BRANDS: [RegExp, BrowserBrand][] = [
  [/CriOS\//, { Chrome: null }],
  [/FxiOS\//, { Firefox: null }],
  [/EdgiOS\//, { Edge: null }],
  [/OPiOS\/|OPT\//, { Opera: null }],
  [/Firefox\//, { Firefox: null }],
  [/EdgA\/|Edg\//, { Edge: null }],
  [/OPR\//, { Opera: null }],
  [/SamsungBrowser\//, { SamsungInternet: null }],
];

/**
 * The two every Chromium and WebKit browser carries, whoever built it. Consulted last,
 * because matching one says only which engine is underneath — a fork that names itself
 * is named by its own token instead.
 */
const ENGINE_BRANDS: [RegExp, BrowserBrand][] = [
  [/Chrome\//, { Chrome: null }],
  [/Safari\//, { Safari: null }],
];

/// Tokens every agent carries whoever built the browser: the engine chain, the platform
/// marker, and the two Chromium ships. A browser that names itself does so with a token
/// that is none of these.
const SHARED_TOKENS = new Set([
  "Mozilla",
  "AppleWebKit",
  "KHTML",
  "Gecko",
  "Chrome",
  "Chromium",
  "Safari",
  "Version",
  "Mobile",
]);

/**
 * The product a browser names itself by, where it names one at all.
 *
 * An agent is a chain of `product/version` tokens and whether the browser's own is among
 * them is the vendor's choice: Vivaldi, Opera, Yandex and DuckDuckGo append theirs, while
 * Brave and Arc ship Chrome's agent unchanged and cannot be told from it. Read here
 * rather than listed, so a browser this frontend has never heard of still arrives under
 * its own name instead of under the one it borrowed.
 *
 * The version is dropped: the canister fixes a description at registration, so a version
 * captured here would sit frozen at whichever build first signed in.
 */
const productToken = (agent: string): string | undefined => {
  // `product/version` pairs only. Bare words are not products — an agent carries several
  // in its parenthesised block, and `(KHTML, like Gecko)` alone would otherwise offer
  // "like" as a browser name.
  const products = [...agent.matchAll(/([A-Za-z][\w.-]*)\/[\w.]+/g)].map(
    ([, name]) => name,
  );
  // The last one, because a browser that names itself appends its token after the
  // engine's and Chromium's.
  return products.filter((name) => !SHARED_TOKENS.has(name)).pop();
};

/** Truncated on a character boundary, because the cap the canister enforces is in bytes. */
const capped = (token: string): string => {
  const encoder = new TextEncoder();
  let capped = token;
  while (encoder.encode(capped).length > MAX_BROWSER_TOKEN_BYTES) {
    capped = capped.slice(0, -1);
  }
  return capped;
};

/**
 * A named variant where this frontend has one, otherwise the browser's own token.
 *
 * Three passes, in this order. A specific token wins outright — `CriOS/` is Chrome on
 * iOS, and no fork borrows it. Failing that, a token the browser named itself by, so a
 * Chromium fork reads as itself rather than as Chrome. Only then the engine tokens every
 * one of them carries.
 */
const brandOf = (agent: string): BrowserBrand => {
  const named = BRANDS.find(([token]) => token.test(agent))?.[1];
  if (named !== undefined) {
    return named;
  }
  const own = productToken(agent);
  if (own !== undefined) {
    return { Other: capped(own) };
  }
  return (
    ENGINE_BRANDS.find(([token]) => token.test(agent))?.[1] ?? {
      Other: capped(agent),
    }
  );
};

const systemOf = (agent: string, touchPoints: number): OperatingSystem => {
  if (/CrOS/.test(agent)) return { ChromeOs: null };
  if (/Android/.test(agent)) return { Android: null };
  if (/iPhone|iPod/.test(agent)) return { Ios: null };
  if (/iPad/.test(agent)) return { Ipados: null };
  // An iPad in desktop mode sends a Mac agent and exposes no hints, so the touch points
  // are the only thing that tells it from a Mac. A Mac reports none.
  if (/Macintosh|Mac OS X/.test(agent))
    return touchPoints > 0 ? { Ipados: null } : { Macos: null };
  if (/Windows/.test(agent)) return { Windows: null };
  if (/Linux|X11/.test(agent)) return { Linux: null };
  return { Other: capped(platformToken(agent) ?? agent) };
};

/**
 * The platform a user agent names, for the systems above that none of the known ones
 * matched: it is the first segment of the first parenthesised block.
 *
 * Barely reachable — `X11` and `Linux` sweep up almost everything the seven above miss,
 * and this code only ever runs inside a browser — but where it is reached the block
 * still names the system, and the whole agent is not a system.
 */
const platformToken = (agent: string): string | undefined => {
  const named = agent
    .match(/\(([^)]*)\)/)?.[1]
    .split(";")[0]
    .trim();
  return named === undefined || named.length === 0 ? undefined : named;
};

const formFactorOf = (
  agent: string,
  system: OperatingSystem,
  hints: { mobile?: boolean; formFactors?: string[] },
): FormFactor => {
  if (hints.formFactors?.includes("Tablet") === true) return { Tablet: null };
  if ("Ipados" in system) return { Tablet: null };
  if (hints.mobile === true) return { Mobile: null };
  if ("Ios" in system) return { Mobile: null };
  if ("Android" in system)
    return /Mobile/.test(agent) ? { Mobile: null } : { Tablet: null };
  if (hints.mobile === false) return { Desktop: null };
  if (
    "Macos" in system ||
    "Windows" in system ||
    "Linux" in system ||
    "ChromeOs" in system
  )
    return { Desktop: null };
  return { Unknown: null };
};

/**
 * The hints an agent cannot supply. Absent off Chromium, and the call can be refused,
 * so every field is optional and a refusal is the same answer as no support.
 */
const highEntropyHints = async (): Promise<{
  mobile?: boolean;
  formFactors?: string[];
  model?: string;
}> => {
  const data = (
    navigator as Navigator & {
      userAgentData?: {
        mobile?: boolean;
        getHighEntropyValues?: (hints: string[]) => Promise<{
          model?: string;
          formFactors?: string[];
          formFactor?: string;
        }>;
      };
    }
  ).userAgentData;
  if (data === undefined) {
    return {};
  }
  try {
    const high = await data.getHighEntropyValues?.(["model", "formFactors"]);
    return {
      mobile: data.mobile,
      // Plural in the current spec, singular in the versions that shipped it first.
      formFactors:
        high?.formFactors ??
        (high?.formFactor === undefined ? undefined : [high.formFactor]),
      // Empty off Android, which reports the field but has no model to put in it.
      model: high?.model === "" ? undefined : high?.model,
    };
  } catch {
    return { mobile: data.mobile };
  }
};

export const describeBrowser = async (): Promise<BrowserDescription> => {
  const agent = navigator.userAgent;
  const hints = await highEntropyHints();
  const os = systemOf(agent, navigator.maxTouchPoints);
  return {
    brand: brandOf(agent),
    os,
    form_factor: formFactorOf(agent, os, hints),
    model: hints.model === undefined ? [] : [capped(hints.model)],
  };
};
