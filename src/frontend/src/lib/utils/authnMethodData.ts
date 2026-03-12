import type {
  AuthnMethodData,
  AuthnMethodSecuritySettings,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { CredentialId } from "$lib/utils/credential-devices";
import { DerEncodedPublicKey } from "@icp-sdk/core/agent";
import {
  fromMnemonicWithoutValidation,
  IC_DERIVATION_PATH,
} from "$lib/utils/recoveryPhrase";

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
  alias?: string;
  pubKey: DerEncodedPublicKey;
  credentialId: CredentialId;
  authenticatorAttachment?: AuthenticatorAttachment;
  origin: string;
  aaguid?: Uint8Array;
}): AuthnMethodData => {
  const metadata: MetadataMapV2 = [
    // The origin in the metadata might not match the origin in the auth method if the origin is longer than 50 characters.
    ["origin", { String: origin }],
  ];
  if (alias !== undefined) {
    metadata.push(["alias", { String: alias }]);
  }
  if (authenticatorAttachment !== undefined) {
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
