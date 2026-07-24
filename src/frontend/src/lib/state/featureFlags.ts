import { writable, type Writable } from "svelte/store";
import { FeatureFlag } from "$lib/utils/featureFlags";
import { getConfiguredFeatureFlag, getPrimaryOrigin } from "$lib/globals";

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

  if (globalThis.window === undefined) {
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
    // A value persisted in localStorage (from the browser console or a
    // `?feature_flag_*` URL param) always wins — leave it untouched.
    if (initializedFeatureFlag.isSet()) {
      return;
    }
    // Apply the deployment-level baseline from the canister deploy args, when
    // the operator configured this flag. Uses `temporaryOverride` so the value
    // is re-derived on every load and never persisted to localStorage.
    const configuredValue = getConfiguredFeatureFlag(name);
    if (configuredValue !== undefined) {
      initializedFeatureFlag.temporaryOverride(configuredValue);
    }
    // Let the flag's init callback layer host-based logic on top of the
    // resolved baseline (callbacks read the current value via `isEnabled()`).
    initCallback?.(initializedFeatureFlag);
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

export const MIN_GUIDED_UPGRADE = createFeatureFlagStore(
  "MIN_GUIDED_UPGRADE",
  false,
);

// Init callback shared by the email-recovery flags. Defaults the flag on for
// the production and beta domains (`id.ai`, `beta.id.ai`) and leaves it off
// everywhere else, but an explicit canister-config value always wins: when the
// operator configured the flag, a `false` stays off on every domain and a
// `true` stays on. We use `temporaryOverride` so the value is re-derived on
// every load and a manual `set()` from the console still takes precedence.
const enableOnIdAiDomains =
  (name: string) =>
  (featureFlag: FeatureFlag): void => {
    if (getConfiguredFeatureFlag(name) !== undefined) {
      return;
    }
    const { hostname } = window.location;
    if (hostname === "id.ai" || hostname === "beta.id.ai") {
      featureFlag.temporaryOverride(true);
    }
  };

/// Recover an identity with a previously bound recovery email (the
/// recover-with-email flow on the `/recovery` sign-in page).
export const EMAIL_RECOVERY = createFeatureFlagStore(
  "EMAIL_RECOVERY",
  false,
  enableOnIdAiDomains("EMAIL_RECOVERY"),
);

/// Set up, replace or remove a recovery email on an identity (the recovery
/// email card in the manage area and the dashboard's set-up smart-action).
export const EMAIL_RECOVERY_SETUP = createFeatureFlagStore(
  "EMAIL_RECOVERY_SETUP",
  false,
  enableOnIdAiDomains("EMAIL_RECOVERY_SETUP"),
);

/// Gates the read-only ("queries-only") access option in the `/continue` and
/// `/cli` sign-in flows. (The `/mcp` flow always offers it, ungated: there the
/// choice is persisted with the access grant and applies to the per-app
/// delegations the server later obtains, while its standing delegation stays
/// full access.) Kept OFF by default — including on id.ai / beta.id.ai —
/// because a queries-only delegation carries `permissions = "queries"` in its
/// canister-signed message, and a relying party's agent must round-trip that
/// field or the replica recomputes a different hash and canister-signature
/// verification fails ("sig not found in the signature tree"). II itself now
/// preserves the field end-to-end, and `@icp-sdk/core` (>= 6) *can* represent
/// it on a `Delegation` instance — but `permissions` is a non-standard ICRC-34
/// extension, so a JS relying party's own signer/client must also know to
/// read it out of the delegation result and pass it through; a dapp whose
/// signer only handles the standard pubkey/expiration/targets fields still
/// drops it and fails closed, same as the Rust agent stack (`ic-agent` /
/// `ic-transport-types`), which does not preserve it at all yet. With the
/// flag off, these two flows send full access. Flip this on once relying
/// parties can reasonably be expected to forward the delegation `permissions`
/// field, regardless of agent language.
export const READ_ONLY_MODE = createFeatureFlagStore("READ_ONLY_MODE", false);

export default {
  DOMAIN_COMPATIBILITY,
  HARDWARE_KEY_TEST,
  DISCOVERABLE_PASSKEY_FLOW,
  ENABLE_ALL_LOCALES,
  GUIDED_UPGRADE,
  MIN_GUIDED_UPGRADE,
  EMAIL_RECOVERY,
  EMAIL_RECOVERY_SETUP,
  READ_ONLY_MODE,
} as Record<string, FeatureFlagStore>;
