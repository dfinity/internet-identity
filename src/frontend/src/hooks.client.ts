import type { ServerInit } from "@sveltejs/kit";
import featureFlags from "$lib/state/featureFlags";
import type { FeatureFlag } from "$lib/utils/featureFlags";
import { get } from "svelte/store";

const FEATURE_FLAG_PREFIX = "feature_flag_";

export const init: ServerInit = async () => {
  // Override feature flags based on search params before any other code
  // including other hooks runs that might depend on these feature flags.
  //
  // Example: ?feature_flag_openid_authentication=true
  const url = new URL(window.location.href);

  for (const [key, value] of url.searchParams.entries()) {
    if (key.startsWith(FEATURE_FLAG_PREFIX)) {
      const flag = key.slice(FEATURE_FLAG_PREFIX.length).toUpperCase();
      if (!(flag in featureFlags)) {
        console.warn(`Invalid feature flag received '${flag}'`);
        continue;
      }
      if (!["true", "false"].includes(value)) {
        console.warn(
          `Invalid value received for feature flag '${flag}', accepted values: true/false`,
        );
        continue;
      }
      featureFlags[flag]?.set(value === "true");
      url.searchParams.delete(key);
    }
  }
  // After a feature flag override has been processed, it's removed from the url
  // to avoid confusing users with unexpected information they don't understand.
  //
  // SvelteKit also has its own `replaceState` method but that can't be used yet
  // since the router isn't initialized yet in the `init` method.
  //
  // SvelteKit will log a warning to the console here in dev mode since we're
  // not using the method supplied by SvelteKit, this can be safely ignored.
  window.history.replaceState(undefined, "", url);
};
