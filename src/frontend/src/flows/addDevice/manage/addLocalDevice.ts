import {
  creationOptions,
  Connection,
  AuthenticatedConnection,
} from "../../../utils/iiConnection";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { WebAuthnIdentity } from "@dfinity/identity";
import { pickDeviceAlias } from "./addDevicePickAlias";
import { withLoader } from "../../../components/loader";
import { renderManage } from "../../manage";
import { displayError } from "../../../components/displayError";

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
 * @param connection authenticated II connection
 * @param userNumber anchor to add the device to
 * @param devices already existing devices
 */
export const addLocalDevice = async (
  connection: AuthenticatedConnection,
  userNumber: bigint,
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
    return renderManage(connection, userNumber);
  }
  const deviceName = await pickDeviceAlias();
  if (deviceName === null) {
    // user clicked "cancel", so we go back to "manage"
    return await renderManage(connection, userNumber);
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
  await renderManage(connection, userNumber);
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
