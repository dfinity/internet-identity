import { get } from "svelte/store";
import { SSO_APP_SCOPE } from "$lib/state/featureFlags";
import { authorizationStore } from "$lib/stores/authorization.store";
import { appAccessScope } from "$lib/utils/openID";

/**
 * Browser-local persistence for the user's choice to tell an org's IdP which
 * app they're signing in to, keyed per (SSO domain, app origin): the choice
 * is a disclosure to that IdP about that app, so neither a different app nor
 * a different SSO inherits it. Made on the SSO sign-in screen (checkbox) and
 * honored by the ceremony-less re-auth paths (returning-user continue,
 * 1-click) where no screen is shown. Absent means not consented, so the
 * default is to disclose nothing.
 */
const CONSENT_STORAGE_PREFIX = "ii:sso-app-scope:";

const consentKey = (ssoDomain: string, appOrigin: string): string =>
  `${CONSENT_STORAGE_PREFIX}${ssoDomain.trim().toLowerCase()}|${appOrigin}`;

export const readAppScopeConsent = (
  ssoDomain: string,
  appOrigin: string,
): boolean => {
  if (typeof localStorage === "undefined") return false;
  return localStorage.getItem(consentKey(ssoDomain, appOrigin)) === "1";
};

export const writeAppScopeConsent = (
  ssoDomain: string,
  appOrigin: string,
  consented: boolean,
): void => {
  if (typeof localStorage === "undefined") return;
  const key = consentKey(ssoDomain, appOrigin);
  if (consented) {
    localStorage.setItem(key, "1");
  } else {
    localStorage.removeItem(key);
  }
};

/**
 * The relying-party origin an app-access scope would disclose, or `undefined`
 * outside the authorize flow (e.g. `/login`, `/mcp`, `/manage`), where there
 * is no app to identify. Read from the authorization context
 * (`effectiveOrigin` — the app's derivation origin, the canonical per-app
 * identity), set during the authorize handshake.
 */
export const currentAppOrigin = (): string | undefined =>
  get(authorizationStore)?.effectiveOrigin;

/**
 * Resolve the app-access scope to forward to the org SSO/IdP identified by
 * `ssoDomain`, or `undefined` when it must not be sent:
 *
 *  - the `SSO_APP_SCOPE` feature flag is off (rollout gate; also hides the
 *    consent checkbox), or
 *  - there's no relying-party origin in context (non-authorize flows), or
 *  - the user hasn't consented to disclosing this app to this SSO's IdP
 *    (the checkbox on the SSO sign-in screen; see
 *    {@link writeAppScopeConsent}).
 *
 * Only ever sent to an IdP the user pointed II at via `ssoDomain` — an IdP
 * that hasn't registered the scope rejects the sign-in with `invalid_scope`,
 * which the user can resolve by unchecking the box.
 */
export const currentSsoAppScope = (ssoDomain: string): string | undefined => {
  if (!get(SSO_APP_SCOPE)) {
    return undefined;
  }
  const appOrigin = currentAppOrigin();
  if (appOrigin === undefined) {
    return undefined;
  }
  if (!readAppScopeConsent(ssoDomain, appOrigin)) {
    return undefined;
  }
  return appAccessScope(appOrigin);
};
