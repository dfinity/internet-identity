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

export const convertToCredentialData = (
  device: Omit<DeviceData, "alias">
): CredentialData => ({
  credentialId: Buffer.from(device.credential_id[0] ?? []),
  pubkey: derFromPubkey(device.pubkey),
  origin: device.origin[0],
});
