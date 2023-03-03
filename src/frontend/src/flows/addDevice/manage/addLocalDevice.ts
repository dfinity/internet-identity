import {
  creationOptions,
  AuthenticatedConnection,
} from "../../../utils/iiConnection";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { WebAuthnIdentity } from "@dfinity/identity";
import { promptDeviceAlias } from "../../../components/alias";
import { withLoader } from "../../../components/loader";
import { displayError } from "../../../components/displayError";
import { setAnchorUsed } from "../../../utils/userNumber";

const displayFailedToAddDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message:
      "We failed to add the new device to this Identity Anchor. Please try again.",
    detail: error.message,
    primaryButton: "Back to manage",
  });

/**
 * Add a new device (i.e. a device connected to the browser the user is
 * currently using, like a YubiKey, or FaceID, or, or. Not meant to be used to
 * add e.g. _another_ browser, macbook or iPhone.)
 * @param userNumber anchor to add the device to
 * @param connection authenticated II connection
 * @param devices already existing devices
 */
export const addLocalDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
): Promise<void> => {
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices),
    });
  } catch (error: unknown) {
    await displayFailedToAddDevice(
      error instanceof Error ? error : unknownError()
    );
    return;
  }
  const deviceName = await promptDeviceAlias({ title: "Add a Trusted Device" });
  if (deviceName === null) {
    // user clicked "cancel", so we return
    return;
  }
  try {
    await withLoader(() =>
      connection.add(
        deviceName,
        { unknown: null },
        { authentication: null },
        newDevice.getPublicKey().toDer(),
        { unprotected: null },
        newDevice.rawId
      )
    );
  } catch (error: unknown) {
    await displayFailedToAddDevice(
      error instanceof Error ? error : unknownError()
    );
  }

  setAnchorUsed(userNumber);
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
