import type {
  AuthnMethodData,
  IdentityInfo,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { bytesToHex } from "@noble/hashes/utils";
import { Authenticated } from "$lib/stores/authentication.store";

export type AccessMethod =
  | { passkey: AuthnMethodData }
  | { openid: OpenIdCredential };

/**
 * Compares two access methods for sorting.
 * OpenID methods come before passkeys if timestamps are equal.
 * Undefined timestamps are considered newer.
 */
export const compareAccessMethods = (
  a: AccessMethod,
  b: AccessMethod,
): -1 | 1 | 0 => {
  const aVal =
    "passkey" in a
      ? a.passkey.last_authentication[0]
      : a.openid.last_usage_timestamp[0];
  const bVal =
    "passkey" in b
      ? b.passkey.last_authentication[0]
      : b.openid.last_usage_timestamp[0];
  if (aVal === bVal) {
    return "openid" in a ? -1 : 1;
  }
  if (bVal === undefined) return 1;
  if (aVal === undefined) return -1;
  return bVal > aVal ? 1 : bVal < aVal ? -1 : 0;
};

/**
 * Converts identity info into a sorted array of access methods.
 * Filters out non-passkey authentication methods.
 */
export const toAccessMethods = (
  identityInfo: Pick<IdentityInfo, "openid_credentials" | "authn_methods">,
): AccessMethod[] =>
  [
    // Reverse to put the latest unused first
    ...(identityInfo.openid_credentials[0] ?? [])
      .map((openid) => ({ openid }) as const)
      .reverse(),
    ...identityInfo.authn_methods
      .filter((passkey) => "WebAuthn" in passkey.authn_method)
      .map((passkey) => ({ passkey }) as const)
      .reverse(),
  ].sort(compareAccessMethods);

/**
 * Returns a unique string key for an access method.
 * Uses credential ID for passkeys and issuer+subject for OpenID.
 */
export const toKey = (accessMethod: AccessMethod): string => {
  if (
    "passkey" in accessMethod &&
    "WebAuthn" in accessMethod.passkey.authn_method
  ) {
    return bytesToHex(
      new Uint8Array(accessMethod.passkey.authn_method.WebAuthn.credential_id),
    );
  }
  if ("openid" in accessMethod) {
    return accessMethod.openid.iss + accessMethod.openid.sub;
  }
  throw new Error("Unknown access method type");
};

/**
 * Checks if the given access method matches the currently authenticated method.
 */
export const isCurrentAccessMethod = (
  authenticated: Pick<Authenticated, "authMethod">,
  accessMethod: AccessMethod,
): boolean => {
  if (
    "passkey" in authenticated.authMethod &&
    "passkey" in accessMethod &&
    "WebAuthn" in accessMethod.passkey.authn_method
  ) {
    return (
      bytesToHex(authenticated.authMethod.passkey.credentialId) ===
      bytesToHex(
        new Uint8Array(
          accessMethod.passkey.authn_method.WebAuthn.credential_id,
        ),
      )
    );
  }
  if ("openid" in authenticated.authMethod && "openid" in accessMethod) {
    return (
      authenticated.authMethod.openid.iss === accessMethod.openid.iss &&
      authenticated.authMethod.openid.sub === accessMethod.openid.sub
    );
  }
  return false;
};
