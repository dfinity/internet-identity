import {
  AuthnMethodData,
  AuthnMethodSecuritySettings,
  MetadataMapV2,
} from "$generated/internet_identity_types";
import { CredentialId } from "$src/utils/credential-devices";
import { readDeviceOrigin } from "$src/utils/iiConnection";
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
  ];
  addOriginToMetadata(metadata);
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
}: {
  alias: string;
  pubKey: DerEncodedPublicKey;
  credentialId: CredentialId;
  authenticatorAttachment?: AuthenticatorAttachment;
}): AuthnMethodData => {
  const metadata: MetadataMapV2 = [["alias", { String: alias }]];
  addOriginToMetadata(metadata);
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

const addOriginToMetadata = (metadata: MetadataMapV2) => {
  const origin = readDeviceOrigin();
  if (origin.length !== 0) {
    metadata.push(["origin", { String: origin[0] }]);
  }
};
