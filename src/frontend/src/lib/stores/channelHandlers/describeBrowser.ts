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
  [/Vivaldi\//, { Vivaldi: null }],
  // Names itself but has no variant of its own, so it travels as the token it gave.
  [/DuckDuckGo\//, { Other: "DuckDuckGo" }],
  [/Chrome\//, { Chrome: null }],
  [/Safari\//, { Safari: null }],
];

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
 * Brave ships a plain Chrome agent on purpose and strips the hints that would give it
 * away, so its own check is the only thing that names it. A browser that hides is worth
 * asking directly, because otherwise its owner sees a row that says Chrome.
 */
const isBrave = async (): Promise<boolean> => {
  const brave = (
    navigator as Navigator & { brave?: { isBrave?: () => Promise<boolean> } }
  ).brave;
  try {
    return (await brave?.isBrave?.()) === true;
  } catch {
    return false;
  }
};

const brandOf = async (agent: string): Promise<BrowserBrand> => {
  if (await isBrave()) {
    return { Brave: null };
  }
  return (
    BRANDS.find(([token]) => token.test(agent))?.[1] ?? { Other: capped(agent) }
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
  return { Other: capped(agent) };
};

const formFactorOf = (
  agent: string,
  system: OperatingSystem,
  hints: { mobile?: boolean; formFactors?: string[] },
): FormFactor => {
  if (hints.formFactors?.includes("Tablet")) return { Tablet: null };
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
    brand: await brandOf(agent),
    os,
    form_factor: formFactorOf(agent, os, hints),
    model: hints.model === undefined ? [] : [capped(hints.model)],
  };
};
