import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

type AuthMethod = LastUsedIdentity["authMethod"];

/**
 * Comparison-only projection of an auth method — the fields that decide
 * whether two credentials represent the "same" or "different" sign-in
 * path from the wizard's perspective. Used for the new (in-flight) side,
 * which doesn't yet have a `sub` / `loginHint` / metadata.
 */
export type MethodDescriptor =
  | { passkey: Record<string, never> }
  | { openid: { iss: string } }
  | { sso: { domain: string } };

/**
 * Returns true when the wizard should show the SwitchAccessMethod
 * confirmation dialog before signing in.
 *
 * Fires on:
 * - Cross-family switches (passkey→openid, openid→sso, etc.)
 * - Within-family openid switches where the issuer differs (Google→Apple)
 * - Within-family sso switches where the discovery domain differs
 *
 * Does NOT fire when:
 * - There is no previous snapshot (first-time user)
 * - Same passkey family (only one passkey "thing" from the wizard's perspective)
 * - Same openid family, same issuer
 * - Same sso family, same domain
 */
export const shouldRequestMethodSwitch = (
  previous: AuthMethod | undefined,
  next: MethodDescriptor,
): boolean => {
  if (previous === undefined) return false;
  if ("passkey" in previous) return !("passkey" in next);
  if ("openid" in previous)
    return !("openid" in next) || previous.openid.iss !== next.openid.iss;
  if ("sso" in previous)
    return !("sso" in next) || previous.sso.domain !== next.sso.domain;
  previous satisfies never;
  return false;
};
