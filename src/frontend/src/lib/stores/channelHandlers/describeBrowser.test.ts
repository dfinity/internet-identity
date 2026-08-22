import { afterEach, describe, expect, it } from "vitest";
import { browserLabel, describeBrowser } from "./describeBrowser";

const CHROME_ANDROID =
  "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36";
const FIREFOX_MAC =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:126.0) Gecko/20100101 Firefox/126.0";
const IPAD_DESKTOP_MODE =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15";

const AGENTS: [string, string, number][] = [
  [
    "Chrome on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/125.0.6422.80 Mobile/15E148 Safari/604.1",
    5,
  ],
  [
    "Firefox on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/126.1 Mobile/15E148 Safari/605.1.15",
    5,
  ],
  [
    "Edge on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 EdgiOS/125.2535.60 Mobile/15E148 Safari/605.1.15",
    5,
  ],
  [
    "Opera on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) OPT/4.4.0 Mobile/15E148 Safari/604.1",
    5,
  ],
  [
    "Safari on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
    5,
  ],
  [
    "Safari on iPad",
    "Mozilla/5.0 (iPad; CPU OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
    5,
  ],
  ["Safari on iPad", IPAD_DESKTOP_MODE, 5],
  ["Safari on Mac", IPAD_DESKTOP_MODE, 0],
  ["Firefox on Mac", FIREFOX_MAC, 0],
  ["Chrome on Android", CHROME_ANDROID, 5],
  [
    "Firefox on Android",
    "Mozilla/5.0 (Android 14; Mobile; rv:126.0) Gecko/126.0 Firefox/126.0",
    5,
  ],
  [
    "Samsung Internet on Android",
    "Mozilla/5.0 (Linux; Android 13; SAMSUNG SM-S918B) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/23.0 Chrome/115.0.0.0 Mobile Safari/537.36",
    5,
  ],
  [
    "Edge on Android",
    "Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36 EdgA/125.0.2535.51",
    5,
  ],
  [
    "DuckDuckGo on Android",
    "Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/125.0.0.0 Mobile DuckDuckGo/5 Safari/537.36",
    5,
  ],
  [
    "Edge on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Edg/125.0.2535.51",
    0,
  ],
  [
    "Opera on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 OPR/110.0.0.0",
    0,
  ],
  [
    "Chrome on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    0,
  ],
  [
    "Vivaldi on Linux",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Vivaldi/6.7.3329.41",
    0,
  ],
  [
    "Chrome on Chromebook",
    "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    0,
  ],
  ["Browser on an unknown device", "curl/8.4.0", 0],
];

const stub = (props: Record<string, unknown>): void => {
  for (const [name, value] of Object.entries(props)) {
    Object.defineProperty(navigator, name, { value, configurable: true });
  }
};

describe("browserLabel", () => {
  it.each(AGENTS)("reads %s", (expected, agent, touchPoints) => {
    expect(browserLabel({ agent, touchPoints })).toBe(expected);
  });

  it("names the device itself when the platform reports a model", () => {
    expect(
      browserLabel({
        agent: CHROME_ANDROID,
        touchPoints: 5,
        model: "SM-S918B",
      }),
    ).toBe("Chrome on SM-S918B");
  });

  it("leaves the label alone when the model is empty", () => {
    expect(
      browserLabel({ agent: CHROME_ANDROID, touchPoints: 5, model: "" }),
    ).toBe("Chrome on Android");
  });

  it("drops a model that would push the name past the canister's limit", () => {
    expect(
      browserLabel({
        agent: CHROME_ANDROID,
        touchPoints: 5,
        model: "M".repeat(200),
      }),
    ).toBe("Chrome on Android");
  });
});

describe("describeBrowser", () => {
  afterEach(() => {
    stub({ userAgentData: undefined });
  });

  it("appends the model the platform reports", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        getHighEntropyValues: () => Promise.resolve({ model: "Pixel 5" }),
      },
    });

    await expect(describeBrowser()).resolves.toBe("Chrome on Pixel 5");
  });

  it("falls back to the platform when no model is available", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        getHighEntropyValues: () => Promise.resolve({ model: "" }),
      },
    });

    await expect(describeBrowser()).resolves.toBe("Chrome on Android");
  });

  it("falls back when the platform refuses the question", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        getHighEntropyValues: () => Promise.reject(new Error("not allowed")),
      },
    });

    await expect(describeBrowser()).resolves.toBe("Chrome on Android");
  });

  it("falls back on a browser without the API", async () => {
    stub({ userAgent: FIREFOX_MAC, maxTouchPoints: 0 });

    await expect(describeBrowser()).resolves.toBe("Firefox on Mac");
  });
});
