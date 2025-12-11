import type {
  AuthnMethodData,
  AuthnMethodSecuritySettings,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { CredentialId } from "$lib/utils/credential-devices";
import { DerEncodedPublicKey } from "@icp-sdk/core/agent";
import { nonNullish } from "@dfinity/utils";
import {
  fromMnemonicWithoutValidation,
  IC_DERIVATION_PATH,
} from "$lib/utils/recoveryPhrase";

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
  aaguid,
}: {
  alias: string;
  pubKey: DerEncodedPublicKey;
  credentialId: CredentialId;
  authenticatorAttachment?: AuthenticatorAttachment;
  origin: string;
  aaguid?: Uint8Array;
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
        aaguid: aaguid !== undefined ? [aaguid] : [],
      },
    },
    security_settings: defaultSecuritySettings(),
    last_authentication: [],
  };
};

export const recoveryAuthnMethodData = async (
  recoveryPhrase: string[],
): Promise<AuthnMethodData> => {
  const identity = await fromMnemonicWithoutValidation(
    recoveryPhrase.join(" "),
    IC_DERIVATION_PATH,
  );
  return {
    metadata: [
      ["alias", { String: "Recovery phrase" }],
      ["usage", { String: "recovery_phrase" }],
    ],
    authn_method: {
      PubKey: {
        pubkey: new Uint8Array(identity.getPublicKey().derKey),
      },
    },
    security_settings: {
      protection: { Unprotected: null },
      purpose: { Recovery: null },
    },
    last_authentication: [],
  };
};

const defaultSecuritySettings = (): AuthnMethodSecuritySettings => {
  return {
    protection: { Unprotected: null },
    purpose: { Authentication: null },
  };
};
