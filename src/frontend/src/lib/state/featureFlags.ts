import { writable, type Writable } from "svelte/store";
import { FeatureFlag } from "$lib/utils/featureFlags";

declare global {
  interface Window {
    __featureFlags: Record<string, FeatureFlag>;
  }
}

type FeatureFlagStore = Writable<boolean> & {
  getFeatureFlag: () => FeatureFlag | undefined;
};

const LOCALSTORAGE_FEATURE_FLAGS_PREFIX = "ii-localstorage-feature-flags__";

const createFeatureFlagStore = (
  name: string,
  defaultValue: boolean,
): FeatureFlagStore => {
  const { subscribe, set, update } = writable(defaultValue);

  // We cannot use browser because this is also imported in our showcase
  if (typeof window === "undefined") {
    return {
      subscribe,
      set,
      update,
      getFeatureFlag: () => undefined,
    };
  }

  // define getter function to pass to flag object
  const get = () => {
    let value: boolean = false;
    update((oldVal) => {
      value = oldVal;
      return oldVal;
    });
    return value;
  };

  // Initialize feature flag object with value from localstorage

  const initializedFeatureFlag: FeatureFlag = new FeatureFlag(
    window.localStorage,
    LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name,
    defaultValue,
    set,
    get,
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

  return {
    subscribe,
    set,
    update,
    getFeatureFlag,
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

export default {
  DOMAIN_COMPATIBILITY,
  OPENID_AUTHENTICATION,
  HARDWARE_KEY_TEST,
} as Record<string, Writable<boolean>>;
