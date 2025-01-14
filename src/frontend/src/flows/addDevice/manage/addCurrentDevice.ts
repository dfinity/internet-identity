import { DeviceData } from "$generated/internet_identity_types";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { inferPasskeyAlias, loadUAParser } from "$src/flows/register";
import { setAnchorUsed } from "$src/storage";
import { authenticatorAttachmentToKeyType } from "$src/utils/authenticatorAttachment";
import {
  AuthenticatedConnection,
  creationOptions,
} from "$src/utils/iiConnection";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isWebAuthnCancel,
  isWebAuthnDuplicateDevice,
} from "$src/utils/webAuthnErrorUtils";
import { WebAuthnIdentity } from "@dfinity/identity";
import { addDeviceSuccess } from "../addDeviceSuccess";

const displayFailedToAddDevice = (error: Error) =>
  displayError({
    title: "Failed to add new device",
    message:
      "We failed to add the new device to this Identity Anchor. Please try again.",
    detail: error.message,
    primaryButton: "Back to manage",
  });

/**
 * Add a new device form the current browser.
 *
 * Used to add a device connected to the browser the user is
 * currently using, like a YubiKey, or FaceID.
 *
 * Used to add current device in the current origin.
 *
 * @param userNumber anchor to add the device to
 * @param connection authenticated II connection
 * @param devices already existing devices
 */
export const addCurrentDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: Omit<DeviceData, "alias">[]
): Promise<void> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices),
    });
  } catch (error: unknown) {
    if (isWebAuthnDuplicateDevice(error)) {
      await displayDuplicateDeviceError({ primaryButton: "Back to manage" });
    } else if (isWebAuthnCancel(error)) {
      await displayCancelError({ primaryButton: "Back to manage" });
    } else {
      await displayFailedToAddDevice(
        error instanceof Error ? error : unknownError()
      );
    }
    return;
  }

  const deviceName = await inferPasskeyAlias({
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

    await addDeviceSuccess({ userNumber, deviceAlias: deviceName });
  } catch (error: unknown) {
    await displayFailedToAddDevice(
      error instanceof Error ? error : unknownError()
    );
  }

  // TODO: Set to default rpId when implementing ID-30
  await setAnchorUsed(userNumber, {
    rpId: null,
    origin: window.location.origin,
  });
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
