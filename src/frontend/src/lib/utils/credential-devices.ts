import type {
  DeviceData,
  DeviceKey,
} from "$lib/generated/internet_identity_types";
import { II_LEGACY_ORIGIN } from "$lib/legacy/constants";
import { DerEncodedPublicKey } from "@dfinity/agent";

export type CredentialId = ArrayBuffer;
export type CredentialData = {
  pubkey: DerEncodedPublicKey;
  credentialId: CredentialId;
  origin?: string;
};

const derFromPubkey = (pubkey: DeviceKey): DerEncodedPublicKey =>
  new Uint8Array(pubkey).buffer as DerEncodedPublicKey;

export const convertToValidCredentialData = (
  device: Omit<DeviceData, "alias">,
): CredentialData | undefined => {
  // In certain cases, e.g. Chrome on Windows 10, an invalid credential id is
  // not ignored but instead will result in a WebAuthn error that prevents a
  // user from authenticating with any of their registered devices.
  //
  // Instead of throwing an error, we return `undefined` to make sure that the
  // device will be filtered out to unblock the user from authenticating.
  if (
    device.credential_id.length !== 1 ||
    device.credential_id[0].length === 0
  ) {
    return;
  }
  return {
    credentialId: Buffer.from(device.credential_id[0]),
    pubkey: derFromPubkey(device.pubkey),
    origin: device.origin[0],
  };
};

/**
 * Helper to encapsulate the logic of finding the RP ID needed when a device will be added.
 *
 * We want to avoid a bad UX when users log in.
 * If the user has multiple devices registered in different origins,
 * it can lead to bad UX when calculating the RP ID.
 *
 * Therefore, we want to avoid devices registered in multiple origins.
 *
 * It checks whether all the devices have the same origin.
 * - If they do, it returns the origin.
 * - If they don't, it returns `undefined`.
 *
 * This function is commonly used along `userSupportsWebauthRoR` to check for ROR support first.
 *
 * @param {Object} params
 * @param {DeviceData[]} params.credentials - The devices to check.
 * @returns {string | undefined} The origin to use when adding a new device.
 * - If `undefined` then no common origin was found. Probalby use `window.origin` or `undefined` for RP ID.
 * - If `string` then the origin can be used to add a new device. Remember to use the hostname only for RP ID.
 */
export const getCredentialsOrigin = ({
  credentials,
}: {
  credentials: Omit<DeviceData, "alias">[];
}): string | undefined => {
  const credentialOrigins = new Set(
    credentials.map((c) => c.origin[0] ?? II_LEGACY_ORIGIN),
  );
  if (credentialOrigins.size === 1) {
    return credentialOrigins.values().next().value;
  }
  return undefined;
};
