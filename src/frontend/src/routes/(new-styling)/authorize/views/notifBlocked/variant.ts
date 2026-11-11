import type {
  BrowserBrand,
  OperatingSystem,
} from "$lib/generated/internet_identity_types";

export type BlockedStepsVariant =
  "chrome" | "chrome-android" | "firefox" | "firefox-android" | "safari";

/**
 * Whose instructions to show for a blocked permission.
 *
 * The system comes before the brand on Android, where every browser hands the last
 * step to the same system settings. A browser with no steps of its own gets
 * Chromium's, which is what the engine under most of them puts in the address bar;
 * that is wrong for none of them in the way Safari's menu-bar route would be.
 *
 * iOS is absent on purpose: notifications are not offered there at all, so a blocked
 * permission never reaches this screen.
 */
export const blockedStepsVariant = (
  brand: BrowserBrand,
  os: OperatingSystem,
): BlockedStepsVariant => {
  const android = "Android" in os;
  if ("Firefox" in brand) {
    return android ? "firefox-android" : "firefox";
  }
  if (android) {
    return "chrome-android";
  }
  if ("Safari" in brand) {
    return "safari";
  }
  return "chrome";
};
