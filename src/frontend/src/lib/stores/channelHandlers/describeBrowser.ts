/**
 * The label a browser gives itself when it registers a session device.
 *
 * Self-reported, so it is something the user reads rather than evidence about where a
 * session came from.
 */

/** Ordered most specific first: every later token also appears in the earlier ones' agents. */
const BROWSERS: [RegExp, string][] = [
  [/CriOS\//, "Chrome"],
  [/FxiOS\//, "Firefox"],
  [/EdgiOS\//, "Edge"],
  [/OPiOS\/|OPT\//, "Opera"],
  [/Firefox\//, "Firefox"],
  [/EdgA\/|Edg\//, "Edge"],
  [/OPR\//, "Opera"],
  [/SamsungBrowser\//, "Samsung Internet"],
  [/Vivaldi\//, "Vivaldi"],
  [/DuckDuckGo\//, "DuckDuckGo"],
  [/Chrome\//, "Chrome"],
  [/Safari\//, "Safari"],
];

const MAX_DEVICE_NAME_BYTES = 128;

const browserOf = (agent: string): string =>
  BROWSERS.find(([token]) => token.test(agent))?.[1] ?? "Browser";

/** Names the device where a device word exists, since that is what its owner calls it. */
const platformOf = (agent: string, touchPoints: number): string => {
  if (/CrOS/.test(agent)) return "Chromebook";
  if (/Android/.test(agent)) return "Android";
  if (/iPhone|iPod/.test(agent)) return "iPhone";
  if (/iPad/.test(agent)) return "iPad";
  // An iPad in desktop mode sends a Mac agent. A Mac reports no touch points.
  if (/Macintosh|Mac OS X/.test(agent)) return touchPoints > 0 ? "iPad" : "Mac";
  if (/Windows/.test(agent)) return "Windows";
  if (/Linux|X11/.test(agent)) return "Linux";
  return "an unknown device";
};

const withinLimit = (label: string): boolean =>
  new TextEncoder().encode(label).length <= MAX_DEVICE_NAME_BYTES;

export const browserLabel = ({
  agent,
  touchPoints,
  model,
}: {
  agent: string;
  touchPoints: number;
  model?: string;
}): string => {
  const browser = browserOf(agent);
  const named = `${browser} on ${model}`;
  return model !== undefined && model !== "" && withinLimit(named)
    ? named
    : `${browser} on ${platformOf(agent, touchPoints)}`;
};

/** Populated on Android, and the only thing that names the device itself. */
const modelOf = async (): Promise<string | undefined> => {
  const userAgentData = (
    navigator as Navigator & {
      userAgentData?: {
        getHighEntropyValues?: (hints: string[]) => Promise<{ model?: string }>;
      };
    }
  ).userAgentData;
  try {
    return (await userAgentData?.getHighEntropyValues?.(["model"]))?.model;
  } catch {
    return undefined;
  }
};

export const describeBrowser = async (): Promise<string> =>
  browserLabel({
    agent: navigator.userAgent,
    touchPoints: navigator.maxTouchPoints,
    model: await modelOf(),
  });
