import { afterEach, describe, expect, it } from "vitest";
import { describeBrowser } from "./describeBrowser";
import type {
  BrowserBrand,
  BrowserDescription,
  FormFactor,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";

const CHROME_ANDROID =
  "Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Mobile Safari/537.36";

const stub = (props: Record<string, unknown>): void => {
  for (const [name, value] of Object.entries(props)) {
    Object.defineProperty(navigator, name, { value, configurable: true });
  }
};

const describing = (
  agent: string,
  maxTouchPoints = 0,
): Promise<BrowserDescription> => {
  stub({ userAgent: agent, maxTouchPoints });
  return describeBrowser();
};

afterEach(() => {
  stub({ userAgentData: undefined });
});

/**
 * Real agent strings throughout: what makes these come out right is how the tokens sit
 * relative to one another, which a hand-made string would not reproduce.
 */
describe("brand", () => {
  /// The six this interface names. Each is read from a token more specific than the
  /// `Chrome/` and `Safari/` every one of them also carries.
  it.each([
    {
      name: "Chrome on iOS, which says CriOS",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/125.0.6422.80 Mobile/15E148 Safari/604.1",
      brand: { Chrome: null } satisfies BrowserBrand,
    },
    {
      name: "Firefox on iOS, which says FxiOS",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) FxiOS/126.1 Mobile/15E148 Safari/605.1.15",
      brand: { Firefox: null } satisfies BrowserBrand,
    },
    {
      name: "Edge on iOS, which says EdgiOS",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 EdgiOS/125.2535.60 Mobile/15E148 Safari/605.1.15",
      brand: { Edge: null } satisfies BrowserBrand,
    },
    {
      name: "Opera on iOS, which says OPT",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) OPT/4.4.0 Mobile/15E148 Safari/604.1",
      brand: { Opera: null } satisfies BrowserBrand,
    },
    {
      name: "Safari, whose own token is the WebKit build",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
      brand: { Safari: null } satisfies BrowserBrand,
    },
    {
      name: "Firefox on a Mac",
      agent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:126.0) Gecko/20100101 Firefox/126.0",
      brand: { Firefox: null } satisfies BrowserBrand,
    },
    {
      name: "Edge on Windows",
      agent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0",
      brand: { Edge: null } satisfies BrowserBrand,
    },
    {
      name: "Opera on Windows",
      agent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 OPR/110.0.0.0",
      brand: { Opera: null } satisfies BrowserBrand,
    },
    {
      name: "Samsung Internet",
      agent:
        "Mozilla/5.0 (Linux; Android 13) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/23.0 Chrome/115.0.0.0 Mobile Safari/537.36",
      brand: { SamsungInternet: null } satisfies BrowserBrand,
    },
    {
      name: "plain Chrome, whose last token is Safari",
      agent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      brand: { Chrome: null } satisfies BrowserBrand,
    },
  ])("names $name", async ({ agent, brand }) => {
    await expect(describing(agent)).resolves.toMatchObject({ brand });
  });

  /// A browser outside the six is named by the token it appends, not by the agent it
  /// borrowed — otherwise every Chromium fork reads as Chrome. Read from the agent
  /// rather than listed, so one this frontend has never heard of still arrives named.
  it.each([
    {
      name: "Vivaldi",
      agent:
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Vivaldi/6.7.3329.41",
      other: "Vivaldi",
    },
    {
      name: "Yandex, which this frontend has never been taught",
      agent:
        "Mozilla/5.0 (Windows NT 10.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 YaBrowser/24.4.1 Safari/537.36",
      other: "YaBrowser",
    },
  ])("names $name by its own token", async ({ agent, other }) => {
    await expect(describing(agent)).resolves.toMatchObject({
      brand: { Other: other },
    });
  });

  /// Brave ships Chrome's agent byte for byte, deliberately. There is no token to find
  /// and nothing to report but Chrome — which is the honest answer, not a gap.
  it("reads a browser that hides itself as the one it imitates", async () => {
    await expect(
      describing(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36",
      ),
    ).resolves.toMatchObject({ brand: { Chrome: null } });
  });

  /// The canister fixes a description at registration, so a version captured here would
  /// sit frozen at whichever build first signed in.
  it("keeps the version out of the name", async () => {
    const { brand } = await describing(
      "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Vivaldi/6.7.3329.41",
    );

    expect(brand).toEqual({ Other: "Vivaldi" });
  });
});

describe("operating system", () => {
  it.each([
    {
      name: "Chromebook",
      agent:
        "Mozilla/5.0 (X11; CrOS x86_64 14541.0.0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      os: { ChromeOs: null } satisfies OperatingSystem,
    },
    {
      name: "Android",
      agent: CHROME_ANDROID,
      os: { Android: null } satisfies OperatingSystem,
    },
    {
      name: "iPhone",
      agent:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
      os: { Ios: null } satisfies OperatingSystem,
    },
    {
      name: "iPad",
      agent:
        "Mozilla/5.0 (iPad; CPU OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1",
      os: { Ipados: null } satisfies OperatingSystem,
    },
    {
      name: "Windows",
      agent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      os: { Windows: null } satisfies OperatingSystem,
    },
    {
      name: "Linux",
      agent:
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      os: { Linux: null } satisfies OperatingSystem,
    },
  ])("reads $name", async ({ agent, os }) => {
    await expect(describing(agent)).resolves.toMatchObject({ os });
  });

  /// An iPad in desktop mode sends a Mac agent and exposes no hints, so the touch points
  /// are the only thing telling it from a Mac. A Mac reports none.
  it.each([
    { name: "a Mac", touchPoints: 0, os: { Macos: null } },
    {
      name: "an iPad pretending to be one",
      touchPoints: 5,
      os: { Ipados: null },
    },
  ])("tells $name apart by its touch points", async ({ touchPoints, os }) => {
    await expect(
      describing(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15",
        touchPoints,
      ),
    ).resolves.toMatchObject({ os });
  });

  /// A platform none of the seven names is still named by the agent, in the first
  /// segment of its parenthesised block — the whole agent is not an operating system.
  it("takes an unknown platform from where the agent states it", async () => {
    await expect(
      describing(
        "Mozilla/5.0 (Haiku; U; Haiku BePC) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      ),
    ).resolves.toMatchObject({ os: { Other: "Haiku" } });
  });
});

describe("form factor", () => {
  it.each([
    {
      name: "a phone from its agent",
      agent: CHROME_ANDROID,
      touchPoints: 5,
      form_factor: { Mobile: null } satisfies FormFactor,
    },
    {
      name: "an Android tablet, which says Android without saying Mobile",
      agent:
        "Mozilla/5.0 (Linux; Android 13; SM-X710) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      touchPoints: 5,
      form_factor: { Tablet: null } satisfies FormFactor,
    },
    {
      name: "a desktop",
      agent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
      touchPoints: 0,
      form_factor: { Desktop: null } satisfies FormFactor,
    },
  ])("reads $name", async ({ agent, touchPoints, form_factor }) => {
    await expect(describing(agent, touchPoints)).resolves.toMatchObject({
      form_factor,
    });
  });
});

describe("client hints", () => {
  const withHints = (high: unknown, mobile = true) => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        mobile,
        getHighEntropyValues: () => Promise.resolve(high),
      },
    });
    return describeBrowser();
  };

  it("takes the model the platform reports", async () => {
    await expect(withHints({ model: "Pixel 5" })).resolves.toMatchObject({
      model: ["Pixel 5"],
    });
  });

  /// Android reports the field with nothing in it off a phone, and an absent model has
  /// to stay absent rather than becoming an empty string in the record.
  it("treats an empty model as no model", async () => {
    await expect(withHints({ model: "" })).resolves.toMatchObject({
      model: [],
    });
  });

  it("takes a stated form factor", async () => {
    await expect(withHints({ formFactors: ["Tablet"] })).resolves.toMatchObject(
      {
        form_factor: { Tablet: null },
      },
    );
  });

  /// The canister names desktops, mobiles and tablets. A device stating anything else is
  /// none of the three, and saying so beats what `mobile` alone would have guessed: a
  /// watch reports `mobile: true` and would read as a phone, an e-reader reports
  /// `mobile: false` and would read as a desktop.
  it.each(["Watch", "XR", "Automotive", "EInk"])(
    "leaves a %s unnamed rather than guessing from `mobile`",
    async (factor) => {
      await expect(withHints({ formFactors: [factor] })).resolves.toMatchObject(
        {
          form_factor: { Unknown: null },
        },
      );
    },
  );

  /// Both stated, so the one the canister can name wins over the one it cannot.
  it("prefers a tablet to an unnameable factor stated beside it", async () => {
    await expect(
      withHints({ formFactors: ["EInk", "Tablet"] }),
    ).resolves.toMatchObject({
      form_factor: { Tablet: null },
    });
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
});

/// The canister refuses a token over its cap, so a resolver never offers one.
describe("token limits", () => {
  it("caps a brand it read off the agent", async () => {
    const { brand } = await describing(
      `Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) ${"B".repeat(200)}/1.0`,
    );

    const token = "Other" in brand ? brand.Other : "";
    expect(new TextEncoder().encode(token).length).toBeLessThanOrEqual(64);
  });

  it("caps a model the platform reported", async () => {
    stub({
      userAgent: CHROME_ANDROID,
      maxTouchPoints: 5,
      userAgentData: {
        mobile: true,
        getHighEntropyValues: () => Promise.resolve({ model: "M".repeat(200) }),
      },
    });

    const { model } = await describeBrowser();
    expect(new TextEncoder().encode(model[0] ?? "").length).toBeLessThanOrEqual(
      64,
    );
  });
});
