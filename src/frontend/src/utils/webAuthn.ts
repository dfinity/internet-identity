import { DeviceData } from "$generated/internet_identity_types";
import { features } from "$src/features";
import {
  DummyIdentity,
  IIWebAuthnIdentity,
  creationOptions,
} from "$src/utils/iiConnection";
import { diagnosticInfo, unknownToString } from "$src/utils/utils";
import { WebAuthnIdentity } from "./webAuthnIdentity";

export const constructIdentity = async ({
  devices,
  rpId,
}: {
  devices?: Array<DeviceData>;
  rpId?: string;
}): Promise<IIWebAuthnIdentity> => {
  const opts = creationOptions(devices, undefined, rpId);

  /* The Identity (i.e. key pair) used when creating the anchor.
   * If the "DUMMY_AUTH" feature is set, we create a dummy identity. The same identity must then be used in iiConnection when authenticating.
   */
  const createIdentity = features.DUMMY_AUTH
    ? () => Promise.resolve(new DummyIdentity())
    : () => WebAuthnIdentity.create({ publicKey: opts });

  try {
    return createIdentity();
  } catch (e: unknown) {
    throw new Error(
      `Failed to create passkey: ${unknownToString(
        e,
        "unknown error"
      )}, ${await diagnosticInfo()}`
    );
  }
};

/**
 * Extract AAGUID from `attestationObject.authData` in `AuthenticatorAttestationResponse`
 * @see https://www.w3.org/TR/webauthn-2/#sctn-attestation
 * @param authData from which to extract AAGUID
 * @returns AAGUID or undefined
 */
export const extractAAGUID = (authData: Uint8Array): string | undefined => {
  if (authData.byteLength < 53) {
    return;
  }
  const aaguid = [...authData.slice(37, 53)]
    .map((byte: number) => byte.toString(16).padStart(2, "0"))
    .join("")
    .replace(/^(.{8})(.{4})(.{4})(.{4})(.{12})$/, "$1-$2-$3-$4-$5");
  if (aaguid !== "00000000-0000-0000-0000-000000000000") {
    return aaguid;
  }
};

/**
 * Lookup details from a list of known AAGUID
 * @param aaguid to lookup
 */
export const lookupAAGUID = async (
  aaguid: string
): Promise<string | undefined> => {
  const knownList = (await import("../assets/passkey_aaguid_data.json"))
    .default;
  return knownList[aaguid as keyof typeof knownList];
};
