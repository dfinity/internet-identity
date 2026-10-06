import { t } from "$lib/stores/locale.store";
import { DomainNotConfiguredError } from "$lib/utils/ssoDiscovery";
import { OAuthProviderError } from "$lib/utils/openID";

/**
 * User-actionable copy for an SSO sign-in failure, or a generic message when
 * there's nothing the user (or their SSO admin) can act on.
 *
 * Provider-misconfiguration details (hostname mismatch, HTTPS, malformed
 * discovery document) take the generic path: they're dev-facing, and the
 * caller logs the raw error for that audience.
 */
export const ssoErrorMessage = (e: unknown, domainInput: string): string => {
  if (e instanceof DomainNotConfiguredError) {
    if (e.reason === "origin-denied") {
      // The org gated this dapp off, so no client can serve this origin.
      return t`Your organization hasn't granted this app access via ${domainInput}.`;
    }
    // `timeout`: discovery never resolved — a wrong domain, or an unreachable
    // or failed discovery fetch (the canister keeps reporting `Pending` until
    // we time out) — so point the SSO admin at the discovery endpoint.
    return t`Couldn't load SSO settings from ${domainInput}. Ask your SSO admin to check that /.well-known/ii-openid-configuration is reachable.`;
  }
  if (e instanceof OAuthProviderError) {
    // `unsupported_response_type` = the SSO app is code-only; II needs
    // the hybrid flow because it verifies JWTs canister-side with no
    // token-endpoint exchange. Spell out the fix so the SSO admin can
    // act on it directly.
    if (e.error === "unsupported_response_type") {
      return t`${domainInput}'s SSO app doesn't allow the hybrid OAuth flow II requires. Ask the SSO admin to enable response_type "id_token code".`;
    }
    if (e.error === "access_denied") {
      return t`${domainInput}'s SSO denied the sign-in.`;
    }
    // Everything else: show the provider's own words if any, else the
    // error code. Useful because these bubble straight from the IdP.
    if (e.errorDescription !== undefined && e.errorDescription.length > 0) {
      return t`${domainInput}'s SSO returned "${e.error}": ${e.errorDescription}`;
    }
    return t`${domainInput}'s SSO returned error "${e.error}".`;
  }
  return t`SSO sign-in for ${domainInput} failed. Please try again.`;
};
