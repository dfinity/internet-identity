import type { DeviceData } from "$lib/generated/internet_identity_types";
import { displayError } from "$lib/templates/displayError";
import { withLoader } from "$lib/templates/loader";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { setAnchorUsed } from "$lib/legacy/storage";
import { authenticatorAttachmentToKeyType } from "$lib/utils/authenticatorAttachment";
import {
  AuthenticatedConnection,
  creationOptions,
} from "$lib/utils/iiConnection";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isWebAuthnCancelError,
  isWebAuthnDuplicateDeviceError,
} from "$lib/utils/webAuthnErrorUtils";
import { WebAuthnIdentity } from "$lib/utils/webAuthnIdentity";
import { nonNullish } from "@dfinity/utils";
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
 * Add a new device from the current browser.
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
  devices: Omit<DeviceData, "alias">[],
  origin: string | undefined,
): Promise<void> => {
  const rpId = nonNullish(origin) ? new URL(origin).hostname : undefined;
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();
  let newDevice: WebAuthnIdentity;
  try {
    newDevice = await WebAuthnIdentity.create({
      publicKey: creationOptions(devices, undefined, rpId),
    });
  } catch (error: unknown) {
    if (isWebAuthnDuplicateDeviceError(error)) {
      await displayDuplicateDeviceError({ primaryButton: "Back to manage" });
    } else if (isWebAuthnCancelError(error)) {
      await displayCancelError({ primaryButton: "Back to manage" });
    } else {
      await displayFailedToAddDevice(
        error instanceof Error ? error : unknownError(),
      );
    }
    return;
  }

  const deviceName = await inferPasskeyAlias({
    authenticatorType: newDevice.getAuthenticatorAttachment(),
    userAgent: navigator.userAgent,
    uaParser,
    aaguid: newDevice.aaguid,
  });
  try {
    await withLoader(() =>
      connection.add(
        deviceName,
        authenticatorAttachmentToKeyType(
          newDevice.getAuthenticatorAttachment(),
        ),
        { authentication: null },
        newDevice.getPublicKey().toDer(),
        { unprotected: null },
        origin ?? window.origin,
        newDevice.rawId,
      ),
    );

    await addDeviceSuccess({ userNumber, deviceAlias: deviceName });
  } catch (error: unknown) {
    await displayFailedToAddDevice(
      error instanceof Error ? error : unknownError(),
    );
  }

  await setAnchorUsed(userNumber);
};

const unknownError = (): Error => {
  return new Error("Unknown error");
};
