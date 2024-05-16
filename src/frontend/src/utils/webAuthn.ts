import { DeviceData } from "$generated/internet_identity_types";
import { features } from "$src/features";
import {
  creationOptions,
  DummyIdentity,
  IIWebAuthnIdentity,
} from "$src/utils/iiConnection";
import { diagnosticInfo, unknownToString } from "$src/utils/utils";
import { WebAuthnIdentity } from "@dfinity/identity";
import { isNullish } from "@dfinity/utils";

export const constructIdentity = async ({
  devices,
}: {
  devices?: () => Promise<Array<DeviceData>>;
}): Promise<IIWebAuthnIdentity> => {
  const opts = isNullish(devices)
    ? creationOptions()
    : creationOptions(await devices());

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
