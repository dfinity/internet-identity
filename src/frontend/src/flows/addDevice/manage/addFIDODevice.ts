import { DeviceData } from "$generated/internet_identity_types";
import { promptDeviceAlias } from "$root/components/alias";
import { displayError } from "$root/components/displayError";
import { withLoader } from "$root/components/loader";
import { authenticatorAttachmentToKeyType } from "$root/utils/authenticatorAttachment";
import {
  AuthenticatedConnection,
  creationOptions,
} from "$root/utils/iiConnection";
import { setAnchorUsed } from "$root/utils/userNumber";
import {
  displayCancelError,
  isCancel,
  isDuplicateDeviceError,
} from "$root/utils/webAuthnErrorUtils";
import { WebAuthnIdentity } from "@dfinity/identity";
import { renderAddDeviceSuccess } from "./addDeviceSuccess";

const displayFailedToAddDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message:
      "We failed to add the new device to this Identity Anchor. Please try again.",
    detail: error.message,
    primaryButton: "Back to manage",
  });

const displayAlreadyRegisteredDevice = () =>
  displayError({
    title: "Duplicate Device",
    message: "This device has already been added to your anchor.",
    detail:
      "Passkeys may be synchronized across devices automatically (e.g. Apple Passkeys) and do not need to be manually added to your Anchor.",
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
export const addFIDODevice = async (
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
    if (isDuplicateDeviceError(error)) {
      await displayAlreadyRegisteredDevice();
    } else if (isCancel(error)) {
      await displayCancelError("Back to manage");
    } else {
      await displayFailedToAddDevice(
        error instanceof Error ? error : unknownError()
      );
    }
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
        authenticatorAttachmentToKeyType(
          newDevice.getAuthenticatorAttachment()
        ),
        { authentication: null },
        newDevice.getPublicKey().toDer(),
        { unprotected: null },
        newDevice.rawId
      )
    );

    await renderAddDeviceSuccess({ deviceAlias: deviceName });
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
