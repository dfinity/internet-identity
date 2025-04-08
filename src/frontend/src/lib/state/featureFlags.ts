import { readable } from "svelte/store";
import { FeatureFlag } from "$lib/utils/featureFlags";

interface FeatureFlags {
  DOMAIN_COMPATIBILITY: FeatureFlag;
  OPENID_AUTHENTICATION: FeatureFlag;
  HARDWARE_KEY_TEST: FeatureFlag;
}

const FEATURE_FLAGS_WITH_DEFAULTS = {
  DOMAIN_COMPATIBILITY: true,
  OPENID_AUTHENTICATION: false,
  HARDWARE_KEY_TEST: false,
} as const satisfies Record<string, boolean>;

const LOCALSTORAGE_FEATURE_FLAGS_PREFIX = "ii-localstorage-feature-flags__";

const featureFlags = readable<FeatureFlags>(undefined, (set) => {
  // We cannot use browser because this is also imported in our showcase
  if (typeof window === "undefined") return;

  // Initialize feature flags with values from localstorage
  const initializedFeatureFlags = Object.fromEntries(
    Object.entries(FEATURE_FLAGS_WITH_DEFAULTS).map(([key, defaultValue]) => [
      key,
      new FeatureFlag(
        window.localStorage,
        LOCALSTORAGE_FEATURE_FLAGS_PREFIX + key,
        defaultValue,
      ),
    ]),
  );
  // Make feature flags configurable from browser console
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  window.__featureFlags = initializedFeatureFlags;
  // Set the initial values
  set(initializedFeatureFlags as unknown as FeatureFlags);
});

export default featureFlags;
