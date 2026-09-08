import { afterEach, describe, expect, it } from "vitest";
import { describeBrowser } from "./describeBrowser";
import type {
  BrowserBrand,
  FormFactor,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";

const CHROME_ANDROID =
  "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36";
const FIREFOX_MAC =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:126.0) Gecko/20100101 Firefox/126.0";
const IPAD_DESKTOP_MODE =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15";

/**
 * Every agent this frontend is expected to recognise, with what it resolves to. The
 * agents are real strings, because the ordering of the brand table is what makes them
 * come out right: a Chromium agent carries `Safari/` and `Chrome/` too.
 */
const AGENTS: [
  string,
  string,
  number,
  BrowserBrand,
  OperatingSystem,
  FormFactor,
][] = [
  [
    "Chrome on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/125.0.6422.80 Mobile/15E148 Safari/604.1",
    5,
    { Chrome: null },
    { Ios: null },
    { Mobile: null },
  ],
  [
    "Firefox on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/126.1 Mobile/15E148 Safari/605.1.15",
    5,
    { Firefox: null },
    { Ios: null },
    { Mobile: null },
  ],
  [
    "Edge on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 EdgiOS/125.2535.60 Mobile/15E148 Safari/605.1.15",
    5,
    { Edge: null },
    { Ios: null },
    { Mobile: null },
  ],
  [
    "Opera on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) OPT/4.4.0 Mobile/15E148 Safari/604.1",
    5,
    { Opera: null },
    { Ios: null },
    { Mobile: null },
  ],
  [
    "Safari on iPhone",
    "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
    5,
    { Safari: null },
    { Ios: null },
    { Mobile: null },
  ],
  [
    "Safari on iPad",
    "Mozilla/5.0 (iPad; CPU OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
    5,
    { Safari: null },
    { Ipados: null },
    { Tablet: null },
  ],
  [
    "Safari on iPad",
    IPAD_DESKTOP_MODE,
    5,
    { Safari: null },
    { Ipados: null },
    { Tablet: null },
  ],
  [
    "Safari on Mac",
    IPAD_DESKTOP_MODE,
    0,
    { Safari: null },
    { Macos: null },
    { Desktop: null },
  ],
  [
    "Firefox on Mac",
    FIREFOX_MAC,
    0,
    { Firefox: null },
    { Macos: null },
    { Desktop: null },
  ],
  [
    "Chrome on Android",
    CHROME_ANDROID,
    5,
    { Chrome: null },
    { Android: null },
    { Mobile: null },
  ],
  [
    "Firefox on Android",
    "Mozilla/5.0 (Android 14; Mobile; rv:126.0) Gecko/126.0 Firefox/126.0",
    5,
    { Firefox: null },
    { Android: null },
    { Mobile: null },
  ],
  [
    "Samsung Internet on Android",
    "Mozilla/5.0 (Linux; Android 13; SAMSUNG SM-S918B) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/23.0 Chrome/115.0.0.0 Mobile Safari/537.36",
    5,
    { SamsungInternet: null },
    { Android: null },
    { Mobile: null },
  ],
  [
    "Edge on Android",
    "Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36 EdgA/125.0.2535.51",
    5,
    { Edge: null },
    { Android: null },
    { Mobile: null },
  ],
  [
    "DuckDuckGo on Android",
    "Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/125.0.0.0 Mobile DuckDuckGo/5 Safari/537.36",
    5,
    { Other: "DuckDuckGo" },
    { Android: null },
    { Mobile: null },
  ],
  [
    "Edge on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Edg/125.0.2535.51",
    0,
    { Edge: null },
    { Windows: null },
    { Desktop: null },
  ],
  [
    "Opera on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 OPR/110.0.0.0",
    0,
    { Opera: null },
    { Windows: null },
    { Desktop: null },
  ],
  [
    "Chrome on Windows",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    0,
    { Chrome: null },
    { Windows: null },
    { Desktop: null },
  ],
  [
    "Vivaldi on Linux",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Vivaldi/6.7.3329.41",
    0,
    { Vivaldi: null },
    { Linux: null },
    { Desktop: null },
  ],
  [
    "Chrome on Chromebook",
    "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    0,
    { Chrome: null },
    { ChromeOs: null },
    { Desktop: null },
  ],
  [
    "Browser on an unknown device",
    "curl/8.4.0",
    0,
    { Other: "curl/8.4.0" },
    { Other: "curl/8.4.0" },
    { Unknown: null },
  ],
];

const stub = (props: Record<string, unknown>): void => {
  for (const [name, value] of Object.entries(props)) {
    Object.defineProperty(navigator, name, { value, configurable: true });
  }
};

describe("describeBrowser", () => {
  afterEach(() => {
    stub({ userAgentData: undefined, brave: undefined });
  });

  it.each(AGENTS)(
    "reads %s",
    async (_label, agent, touchPoints, brand, os, form_factor) => {
      stub({ userAgent: agent, maxTouchPoints: touchPoints });

      await expect(describeBrowser()).resolves.toEqual({
        brand,
        os,
        form_factor,
        model: [],
      });
    },
  );

  it("takes the model the platform reports", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        mobile: true,
        getHighEntropyValues: () => Promise.resolve({ model: "Pixel 5" }),
      },
    });

    await expect(describeBrowser()).resolves.toMatchObject({
      model: ["Pixel 5"],
    });
  });

  /// Android reports the field with nothing in it off a phone, and an absent model has
  /// to stay absent rather than becoming an empty string in the record.
  it("treats an empty model as no model", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        mobile: true,
        getHighEntropyValues: () => Promise.resolve({ model: "" }),
      },
    });

    await expect(describeBrowser()).resolves.toMatchObject({ model: [] });
  });

  it("still describes the browser when the platform refuses the question", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        mobile: true,
        getHighEntropyValues: () => Promise.reject(new Error("not allowed")),
      },
    });

    await expect(describeBrowser()).resolves.toEqual({
      brand: { Chrome: null },
      os: { Android: null },
      form_factor: { Mobile: null },
      model: [],
    });
  });

  /// The current spec says `formFactors`; the versions that shipped it first said
  /// `formFactor`. A tablet has to read as one on both.
  it("takes a stated form factor in either shape", async () => {
    for (const high of [
      { formFactors: ["Tablet"] },
      { formFactor: "Tablet" },
    ]) {
      stub({
        userAgent: CHROME_ANDROID,
        maxTouchPoints: 5,
        userAgentData: {
          mobile: true,
          getHighEntropyValues: () => Promise.resolve(high),
        },
      });

      await expect(describeBrowser()).resolves.toMatchObject({
        form_factor: { Tablet: null },
      });
    }
  });

  /// Brave sends a plain Chrome agent and strips the hints that would give it away, so
  /// without asking it directly its owner sees a row that says Chrome.
  it("names Brave, which its agent does not", async () => {
    stub({
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      maxTouchPoints: 0,
      brave: { isBrave: () => Promise.resolve(true) },
    });

    await expect(describeBrowser()).resolves.toMatchObject({
      brand: { Brave: null },
      os: { Macos: null },
    });
  });

  it("reads a browser that refuses the Brave question as its agent says", async () => {
    stub({
      userAgent: FIREFOX_MAC,
      maxTouchPoints: 0,
      brave: {
        isBrave: () => Promise.reject(new Error("no")),
      },
    });

    await expect(describeBrowser()).resolves.toMatchObject({
      brand: { Firefox: null },
    });
  });

  /// The canister refuses a token over its cap, so a resolver must never offer one.
  it("caps a token it did not recognise", async () => {
    stub({ userAgent: "x".repeat(500), maxTouchPoints: 0 });

    const description = await describeBrowser();
    const token = "Other" in description.brand ? description.brand.Other : "";
    expect(new TextEncoder().encode(token).length).toBeLessThanOrEqual(64);
  });
});
