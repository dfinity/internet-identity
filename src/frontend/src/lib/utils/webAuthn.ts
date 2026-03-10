import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
import { bufferEqual } from "$lib/utils/iiConnection";
import { uint8ArrayEqual } from "./utils";

/**
 * Extract AAGUID from `attestationObject.authData` in `AuthenticatorAttestationResponse`
 * @see https://www.w3.org/TR/webauthn-2/#sctn-attestation
 * @param authData from which to extract AAGUID
 * @returns AAGUID or undefined
 */
export const extractAAGUID = (authData: Uint8Array): Uint8Array | undefined => {
  if (authData.byteLength < 53) {
    return;
  }
  const aaguid = authData.slice(37, 53);
  if (aaguid.every((byte) => byte === 0)) {
    return undefined;
  }
  return aaguid;
};

/**
 * Convert raw AAGUID bytes into its standardized human-readable UUID string
 *
 * @param aaguid in raw bytes
 * @returns human-readable UUID string
 */
export const aaguidToString = (aaguid: Uint8Array): string =>
  [...aaguid]
    .map((byte: number) => byte.toString(16).padStart(2, "0"))
    .join("")
    .replace(/^(.{8})(.{4})(.{4})(.{4})(.{12})$/, "$1-$2-$3-$4-$5");

/**
 * Check if two `AuthnMethodData` values are equal to one another
 */
export const authnMethodEqual = (
  a: AuthnMethodData,
  b: AuthnMethodData,
): boolean => {
  if ("WebAuthn" in a.authn_method && "WebAuthn" in b.authn_method) {
    return uint8ArrayEqual(
      new Uint8Array(a.authn_method.WebAuthn.pubkey),
      new Uint8Array(b.authn_method.WebAuthn.pubkey),
    );
  }
  if ("PubKey" in a.authn_method && "PubKey" in b.authn_method) {
    return uint8ArrayEqual(
      new Uint8Array(a.authn_method.PubKey.pubkey),
      new Uint8Array(b.authn_method.PubKey.pubkey),
    );
  }
  return false;
};

export const authnMethodToPublicKey = (
  authnMethod: AuthnMethodData,
): Uint8Array => {
  if ("WebAuthn" in authnMethod.authn_method) {
    return new Uint8Array(authnMethod.authn_method.WebAuthn.pubkey);
  }
  if ("PubKey" in authnMethod.authn_method) {
    return new Uint8Array(authnMethod.authn_method.PubKey.pubkey);
  }
  throw new Error(
    `Unknown authentication method: ${JSON.stringify(authnMethod.authn_method)}`,
  );
};

export const getAuthnMethodAlias = (authnMethod: AuthnMethodData): string => {
  const metadataAlias = authnMethod.metadata.find(
    ([key, _val]) => key === "alias",
  )?.[1];
  if (metadataAlias && "String" in metadataAlias) {
    return metadataAlias.String;
  }
  return "";
};
