import type {
  AuthnMethodData,
  AuthnMethodSecuritySettings,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { CredentialId } from "$lib/utils/credential-devices";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { nonNullish } from "@dfinity/utils";

/**
 * Helper to create a new PinIdentity authn method to be used in a registration flow.
 */
export const pinAuthnMethodData = ({
  alias,
  pubKey,
}: {
  alias: string;
  pubKey: DerEncodedPublicKey;
}): AuthnMethodData => {
  const metadata: MetadataMapV2 = [
    ["alias", { String: alias }],
    ["usage", { String: "browser_storage_key" }],
    ["origin", { String: window.origin }],
  ];
  return {
    metadata,
    authn_method: {
      PubKey: { pubkey: new Uint8Array(pubKey) },
    },
    security_settings: defaultSecuritySettings(),
    last_authentication: [],
  };
};

/**
 * Helper to create a new passkey authn method to be used in a registration flow.
 */
export const passkeyAuthnMethodData = ({
  alias,
  pubKey,
  credentialId,
  authenticatorAttachment,
  origin,
}: {
  alias: string;
  pubKey: DerEncodedPublicKey;
  credentialId: CredentialId;
  authenticatorAttachment?: AuthenticatorAttachment;
  origin: string;
}): AuthnMethodData => {
  const metadata: MetadataMapV2 = [
    ["alias", { String: alias }],
    // The origin in the metadata might not match the origin in the auth method if the origin is longer than 50 characters.
    ["origin", { String: origin }],
  ];
  if (nonNullish(authenticatorAttachment)) {
    metadata.push([
      "authenticator_attachment",
      { String: authenticatorAttachment },
    ]);
  }
  return {
    metadata,
    authn_method: {
      WebAuthn: {
        pubkey: new Uint8Array(pubKey),
        credential_id: new Uint8Array(credentialId),
      },
    },
    security_settings: defaultSecuritySettings(),
    last_authentication: [],
  };
};

const defaultSecuritySettings = (): AuthnMethodSecuritySettings => {
  return {
    protection: { Unprotected: null },
    purpose: { Authentication: null },
  };
};
