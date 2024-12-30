import { DeviceData, DeviceKey } from "$generated/internet_identity_types";
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
  device: Omit<DeviceData, "alias">
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
