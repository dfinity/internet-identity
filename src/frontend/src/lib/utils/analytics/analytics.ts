import type { AnalyticsConfig } from "$lib/generated/internet_identity_types";
import { authorizationStore } from "$lib/stores/authorization.store";
import Plausible from "plausible-tracker";
import { PlausibleInitOptions } from "plausible-tracker/build/main/lib/tracker";
import { get } from "svelte/store";

let tracker: undefined | ReturnType<typeof Plausible>;

const convertToPlausibleConfig = (
  config: AnalyticsConfig | undefined,
): PlausibleInitOptions | undefined => {
  if (config === undefined) {
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
  if (config === undefined) {
    return;
  }
  const plausibleConfig = convertToPlausibleConfig(config);
  if (plausibleConfig === undefined) {
    return;
  }
  tracker = Plausible(plausibleConfig);
};

export const analytics = {
  pageView: () => {
    tracker?.trackPageview();
  },
  event: (name: string, props?: Record<string, string | number | boolean>) => {
    const state = get(authorizationStore);
    const authorizationOrigin = state?.effectiveOrigin;

    tracker?.trackEvent(name, {
      props:
        authorizationOrigin !== undefined
          ? { ...props, authorizationOrigin }
          : props,
    });
  },
};
