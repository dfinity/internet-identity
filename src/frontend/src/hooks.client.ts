import type { ClientInit } from "@sveltejs/kit";
import featureFlags from "$lib/state/featureFlags";
import { authenticationStore } from "$lib/stores/authentication.store";
import { sessionStore } from "$lib/stores/session.store";
import {
  initGlobals,
  canisterId,
  agentOptions,
  canisterConfig,
} from "$lib/globals";
import { isNullish } from "@dfinity/utils";
import { isSameOrigin } from "$lib/utils/urlUtils";

const FEATURE_FLAG_PREFIX = "feature_flag_";

const overrideFeatureFlags = () => {
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

// Set the discoverable passkey flag based on the new flow origins config.
// This will be used for id.ai and Utopia to enable the new authentication flow.
// Once the feature is enabled for all users, this can be removed.
const maybeSetDiscoverablePasskeyFlowFlag = () => {
  const newFlowOrigins = canisterConfig.new_flow_origins[0];
  if (isNullish(newFlowOrigins)) {
    return;
  }
  const origin = window.location.origin;
  if (newFlowOrigins.filter((o) => isSameOrigin(o, origin)).length === 0) {
    return;
  }
  featureFlags.DISCOVERABLE_PASSKEY_FLOW.set(true);
};

export const init: ClientInit = async () => {
  overrideFeatureFlags();
  initGlobals();
  maybeSetDiscoverablePasskeyFlowFlag();
  await sessionStore.init({ canisterId, agentOptions });
  authenticationStore.init({ canisterId, agentOptions });
};
