import { describe, expect, it } from "vitest";
import { blockedStepsVariant } from "./variant";

const CHROME = { Chrome: null } as const;
const FIREFOX = { Firefox: null } as const;
const SAFARI = { Safari: null } as const;
const MACOS = { Macos: null } as const;
const ANDROID = { Android: null } as const;

describe("blockedStepsVariant", () => {
  it("shows Chromium's address-bar route on the desktop browsers built on it", () => {
    expect(blockedStepsVariant(CHROME, { Windows: null })).toBe("chrome");
    expect(blockedStepsVariant({ Edge: null }, MACOS)).toBe("chrome");
    expect(blockedStepsVariant({ Opera: null }, { Linux: null })).toBe(
      "chrome",
    );
  });

  /** Brave, Arc and Vivaldi ship Chrome's agent or their own token, and all put the
   *  same control in the address bar. */
  it("shows them for a browser it has never heard of", () => {
    expect(blockedStepsVariant({ Other: "Vivaldi" }, MACOS)).toBe("chrome");
  });

  it("shows Safari's menu-bar route only on the desktop", () => {
    expect(blockedStepsVariant(SAFARI, MACOS)).toBe("safari");
  });

  it("shows Firefox's own route, per system", () => {
    expect(blockedStepsVariant(FIREFOX, MACOS)).toBe("firefox");
    expect(blockedStepsVariant(FIREFOX, ANDROID)).toBe("firefox-android");
  });

  /** Android hands the last step to the system settings whichever browser asked, so
   *  the system decides before the brand does. */
  it("prefers the system on Android for every brand but Firefox", () => {
    expect(blockedStepsVariant(CHROME, ANDROID)).toBe("chrome-android");
    expect(blockedStepsVariant({ SamsungInternet: null }, ANDROID)).toBe(
      "chrome-android",
    );
    expect(blockedStepsVariant({ Other: "Brave" }, ANDROID)).toBe(
      "chrome-android",
    );
  });
});
