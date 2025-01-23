import { DeviceData } from "$generated/internet_identity_types";
import { features } from "$src/features";
import {
  DummyIdentity,
  IIWebAuthnIdentity,
  creationOptions,
} from "$src/utils/iiConnection";
import { diagnosticInfo, unknownToString } from "$src/utils/utils";
import { WebAuthnIdentity } from "@dfinity/identity";

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
