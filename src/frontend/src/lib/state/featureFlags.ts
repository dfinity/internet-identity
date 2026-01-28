import { writable, type Writable } from "svelte/store";
import { FeatureFlag } from "$lib/utils/featureFlags";
import { isNullish, nonNullish } from "@dfinity/utils";
import { getPrimaryOrigin } from "$lib/globals";

declare global {
  interface Window {
    __featureFlags: Record<string, FeatureFlag>;
  }
}

type FeatureFlagStore = Writable<boolean> & {
  getFeatureFlag: () => FeatureFlag | undefined;
  initialize: () => void;
};

const LOCALSTORAGE_FEATURE_FLAGS_PREFIX = "ii-localstorage-feature-flags__";

const createFeatureFlagStore = (
  name: string,
  defaultValue: boolean,
  initCallback?: (featureFlag: FeatureFlag) => void,
): FeatureFlagStore => {
  const { subscribe, set, update } = writable(defaultValue);

  // We cannot use browser because this is also imported in our showcase
  if (isNullish(globalThis.window)) {
    return {
      subscribe,
      set,
      update,
      getFeatureFlag: () => undefined,
      initialize: () => undefined,
    };
  }

  // Initialize feature flag object with value from localstorage
  const initializedFeatureFlag = new FeatureFlag(
    window.localStorage,
    LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name,
    defaultValue,
    { subscribe, set, update },
  );

  // Make feature flags configurable from browser console
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  if (typeof window.__featureFlags === "undefined") {
    window.__featureFlags = {};
  }
  window.__featureFlags[name] = initializedFeatureFlag;

  const getFeatureFlag = () => {
    return initializedFeatureFlag;
  };
  const initialize = (): void => {
    // Call init callback if not already set
    if (nonNullish(initCallback) && !initializedFeatureFlag.isSet()) {
      initCallback?.(initializedFeatureFlag);
    }
  };

  return {
    subscribe,
    set,
    update,
    getFeatureFlag,
    initialize,
  };
};

export const DOMAIN_COMPATIBILITY = createFeatureFlagStore(
  "DOMAIN_COMPATIBILITY",
  true,
);

export const HARDWARE_KEY_TEST = createFeatureFlagStore(
  "HARDWARE_KEY_TEST",
  false,
);

export const DISCOVERABLE_PASSKEY_FLOW = createFeatureFlagStore(
  "DISCOVERABLE_PASSKEY_FLOW",
  true,
);

export const ENABLE_ALL_LOCALES = createFeatureFlagStore(
  "ENABLE_ALL_LOCALES",
  false,
);

export const GUIDED_UPGRADE = createFeatureFlagStore(
  "GUIDED_UPGRADE",
  false,
  // Enable guide upgrade flow if page is visited from domain other than id.ai
  (featureFlag) => {
    const primaryOrigin = getPrimaryOrigin();
    if (
      primaryOrigin !== undefined &&
      primaryOrigin !== window.location.origin
    ) {
      featureFlag.temporaryOverride(true);
    }
  },
);

export default {
  DOMAIN_COMPATIBILITY,
  HARDWARE_KEY_TEST,
  DISCOVERABLE_PASSKEY_FLOW,
  ENABLE_ALL_LOCALES,
  GUIDED_UPGRADE,
} as Record<string, FeatureFlagStore>;
