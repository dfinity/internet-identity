import { get } from "svelte/store";
import { SSO_APP_SCOPE } from "$lib/state/featureFlags";
import { authorizationStore } from "$lib/stores/authorization.store";
import { appAccessScope } from "$lib/utils/openID";

/**
 * Resolve the app-access scope to forward to the org SSO/IdP for the current
 * authorize flow, or `undefined` when it shouldn't be sent.
 *
 * Returns `undefined` when:
 *  - the `SSO_APP_SCOPE` feature flag is off, or
 *  - there's no relying-party origin in context (e.g. non-authorize flows such
 *    as `/login`, `/mcp`, `/manage`), so there's no app to identify.
 *
 * The relying-party origin is read from the authorization context
 * (`effectiveOrigin` — the app's derivation origin, the canonical per-app
 * identity), set during the authorize handshake.
 *
 * Prototype (Phase 1): gated purely by the feature flag. A later revision will
 * additionally require the provider to opt in via its discovery document, so
 * the scope is only sent to IdPs configured to accept it (an unregistered
 * scope otherwise fails the sign-in with `invalid_scope`).
 */
export const currentSsoAppScope = (): string | undefined => {
  if (!get(SSO_APP_SCOPE)) {
    return undefined;
  }
  const effectiveOrigin = get(authorizationStore)?.effectiveOrigin;
  return effectiveOrigin !== undefined
    ? appAccessScope(effectiveOrigin)
    : undefined;
};
