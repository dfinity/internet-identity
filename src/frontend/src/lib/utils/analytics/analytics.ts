import type { AnalyticsConfig } from "$lib/generated/internet_identity_types";
import { isNullish } from "@dfinity/utils";
import Plausible from "plausible-tracker";
import { PlausibleInitOptions } from "plausible-tracker/build/main/lib/tracker";

let tracker: undefined | ReturnType<typeof Plausible>;

const convertToPlausibleConfig = (
  config: AnalyticsConfig | undefined,
): PlausibleInitOptions | undefined => {
  if (isNullish(config)) {
    return;
  }
  if ("Plausible" in config) {
    const plausibleConfig = config.Plausible;
    // `undefined` values in Plausible config are not the same as missing values.
    return removeUndefinedFields({
      hashMode: plausibleConfig.hash_mode[0],
      domain: plausibleConfig.domain[0],
      trackLocalhost: plausibleConfig.track_localhost[0],
      apiHost: plausibleConfig.api_host[0],
    });
  }
};

const removeUndefinedFields = (obj: Record<string, unknown | undefined>) => {
  return Object.fromEntries(
    Object.entries(obj).filter(([_, value]) => value !== undefined),
  );
};

export const initAnalytics = (config: AnalyticsConfig | undefined) => {
  if (isNullish(config)) {
    return;
  }
  const plausibleConfig = convertToPlausibleConfig(config);
  if (isNullish(plausibleConfig)) {
    return;
  }
  tracker = Plausible(plausibleConfig);
};

export const analytics = {
  pageView: () => {
    tracker?.trackPageview();
  },
  event: (name: string, props?: Record<string, string | number | boolean>) => {
    tracker?.trackEvent(name, { props });
  },
};
