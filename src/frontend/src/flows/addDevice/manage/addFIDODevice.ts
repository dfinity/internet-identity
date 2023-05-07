import { DeviceData } from "$generated/internet_identity_types";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { inferAlias, loadUAParser } from "$src/flows/register";
import { authenticatorAttachmentToKeyType } from "$src/utils/authenticatorAttachment";
import {
  AuthenticatedConnection,
  creationOptions,
} from "$src/utils/iiConnection";
import { setAnchorUsed } from "$src/utils/userNumber";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isCancel,
  isDuplicateDeviceError,
} from "$src/utils/webAuthnErrorUtils";
import { WebAuthnIdentity } from "@dfinity/identity";
import { addDeviceSuccess } from "./addDeviceSuccess";

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
export const addFIDODevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
): Promise<void> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices),
    });
  } catch (error: unknown) {
    if (isDuplicateDeviceError(error)) {
      await displayDuplicateDeviceError({ primaryButton: "Back to manage" });
    } else if (isCancel(error)) {
      await displayCancelError({ primaryButton: "Back to manage" });
    } else {
      await displayFailedToAddDevice(
        error instanceof Error ? error : unknownError()
      );
    }
    return;
  }

  const deviceName = await inferAlias({
    authenticatorType: newDevice.getAuthenticatorAttachment(),
    userAgent: navigator.userAgent,
    uaParser,
  });
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

    await addDeviceSuccess({ deviceAlias: deviceName });
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
