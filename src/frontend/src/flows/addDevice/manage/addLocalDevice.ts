// Add a new device (i.e. a device connected to the device the user is
// currently using, like a YubiKey, or FaceID, or, or. Not meant to be used to
// add e.g. _another_ macbook or iPhone.)
import { creationOptions, IIConnection } from "../../../utils/iiConnection";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { WebAuthnIdentity } from "@dfinity/identity";
import { pickDeviceAlias } from "./addDevicePickAlias";
import { withLoader } from "../../../components/loader";
import { renderManage } from "../../manage";
import { displayError } from "../../../components/displayError";

const displayFailedToAddNewDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message: "We failed to add your new device.",
    detail: error.message,
    primaryButton: "Back to manage",
  });

const displayFailedToAddTheDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message:
      "We failed to add the new device to this Identity Anchor. Please try again.",
    detail: error.message,
    primaryButton: "Back to manage",
  });

export const addLocalDevice = async (
  userNumber: bigint,
  connection: IIConnection,
  devices: DeviceData[]
): Promise<void> => {
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices),
    });
  } catch (error: unknown) {
    await displayFailedToAddNewDevice(
      error instanceof Error ? error : unknownError()
    );
    return renderManage(userNumber, connection);
  }
  const deviceName = await pickDeviceAlias();
  if (deviceName === null) {
    return renderManage(userNumber, connection);
  }
  try {
    await withLoader(() =>
      connection.add(
        userNumber,
        deviceName,
        { unknown: null },
        { authentication: null },
        newDevice.getPublicKey().toDer(),
        newDevice.rawId
      )
    );
  } catch (error: unknown) {
    await displayFailedToAddTheDevice(
      error instanceof Error ? error : unknownError()
    );
  }
  await renderManage(userNumber, connection);
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
