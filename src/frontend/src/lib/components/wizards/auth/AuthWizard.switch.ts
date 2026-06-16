import type { MethodTag } from "$lib/flows/authFlow.svelte";
import type { LastUsedIdentity } from "$lib/stores/last-used-identities.store";

export type ProviderInfo = {
  providerIssuer?: string;
  providerDomain?: string;
  providerName?: string;
};

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
 * - Same openid family, same or unknown issuer
 * - Same sso family, same or unknown domain
 */
export const shouldRequestMethodSwitch = (
  newMethod: MethodTag,
  previousSnapshot: LastUsedIdentity | undefined,
  providerInfo?: ProviderInfo,
): boolean => {
  if (previousSnapshot === undefined) return false;

  const prev = previousSnapshot.authMethod;

  if ("passkey" in prev) {
    return newMethod !== "passkey";
  }

  if ("openid" in prev) {
    if (newMethod !== "openid") return true;
    const newIssuer = providerInfo?.providerIssuer;
    if (newIssuer === undefined) return false;
    return prev.openid.iss !== newIssuer;
  }

  if ("sso" in prev) {
    if (newMethod !== "sso") return true;
    const newDomain = providerInfo?.providerDomain;
    if (newDomain === undefined) return false;
    return prev.sso.domain !== newDomain;
  }

  void (prev satisfies never);
  return false;
};
