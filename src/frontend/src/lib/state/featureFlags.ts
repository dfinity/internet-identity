import { writable, type Writable } from "svelte/store";
import { FeatureFlag } from "$lib/utils/featureFlags";
import { isNullish, nonNullish } from "@dfinity/utils";
import { canisterConfig } from "$lib/globals";

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
  getInitValue?: () => boolean | undefined,
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
    // Override feature flag with init method if not already set
    if (nonNullish(getInitValue) && !initializedFeatureFlag.isSet()) {
      initializedFeatureFlag.temporaryOverride(getInitValue() ?? defaultValue);
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

export const OPENID_AUTHENTICATION = createFeatureFlagStore(
  "OPENID_AUTHENTICATION",
  false,
);

export const HARDWARE_KEY_TEST = createFeatureFlagStore(
  "HARDWARE_KEY_TEST",
  false,
);

export const DISCOVERABLE_PASSKEY_FLOW = createFeatureFlagStore(
  "DISCOVERABLE_PASSKEY_FLOW",
  false,
);

export const CONTINUE_FROM_ANOTHER_DEVICE = createFeatureFlagStore(
  "CONTINUE_FROM_ANOTHER_DEVICE",
  true, // Enable temp key flow (until backend changes can be enabled)
  () => canisterConfig.feature_flag_continue_from_another_device[0],
);

export const AUTH_FLOW_UPDATES = createFeatureFlagStore(
  "AUTH_FLOW_UPDATES",
  false,
);

export default {
  DOMAIN_COMPATIBILITY,
  OPENID_AUTHENTICATION,
  HARDWARE_KEY_TEST,
  DISCOVERABLE_PASSKEY_FLOW,
  CONTINUE_FROM_ANOTHER_DEVICE,
  AUTH_FLOW_UPDATES,
} as Record<string, FeatureFlagStore>;
