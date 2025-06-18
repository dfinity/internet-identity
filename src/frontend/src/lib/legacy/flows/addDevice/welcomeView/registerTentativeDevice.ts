import type {
  AddTentativeDeviceResponse,
  CredentialId,
  DeviceData,
} from "$lib/generated/internet_identity_types";
import { displayError } from "$lib/templates/displayError";
import { withLoader } from "$lib/templates/loader";
import { addDeviceSuccess } from "$lib/legacy/flows/addDevice/addDeviceSuccess";
import { tentativeDeviceStepper } from "$lib/legacy/flows/addDevice/stepper";
import { promptDeviceTrusted } from "$lib/legacy/flows/addDevice/welcomeView/promptDeviceTrusted";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { setAnchorUsed } from "$lib/legacy/storage";
import { authenticatorAttachmentToKeyType } from "$lib/utils/authenticatorAttachment";
import { Connection, creationOptions } from "$lib/utils/iiConnection";
import { unknownToString, unreachable, unreachableLax } from "$lib/utils/utils";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isWebAuthnCancelError,
  isWebAuthnDuplicateDeviceError,
} from "$lib/utils/webAuthnErrorUtils";
import { WebAuthnIdentity } from "$lib/utils/webAuthnIdentity";
import { deviceRegistrationDisabledInfo } from "./deviceRegistrationModeDisabled";
import { showVerificationCode } from "./showVerificationCode";

/**
 * Runs the tentative device registration flow on a _new_ device:
 *  1. The user is prompted to confirm this device is trusted.
 *  2. The user interacts with the authenticator to create a new credential.
 *  3. After adding it tentatively, the user is prompted to enter the verification
 *     code on an existing device.
 *  4. This flows polls for the user to complete the verification.
 *  5. Once verification is completed, a success screen is shown.
 *
 *  If the user cancels at any point, the flow is aborted.
 *
 * @param userNumber anchor to add the tentative device to.
 * @param connection connection to interact with the II canister.
 */
export const registerTentativeDevice = async (
  userNumber: bigint,
  connection: Connection,
): Promise<{ tag: "deviceAdded" } | { tag: "canceled" }> => {
  // Kick-off fetching "ua-parser-js";
  const uaParser = loadUAParser();

  const deviceTrusted = await promptDeviceTrusted({ userNumber });
  if (deviceTrusted === "canceled") {
    return { tag: "canceled" };
  }
  deviceTrusted satisfies "confirmed";

  // Then, we create local WebAuthn credentials for the device
  const result = await withLoader(() =>
    createDevice({ userNumber, connection }),
  );

  if (result instanceof Error) {
    if (isWebAuthnDuplicateDeviceError(result)) {
      // Given that this is a remote device where we get the result that authentication should work,
      // let's help the user and fill in their anchor number.
      await setAnchorUsed(userNumber);
      await displayDuplicateDeviceError({ primaryButton: "Ok" });
    } else if (isWebAuthnCancelError(result)) {
      await displayCancelError({ primaryButton: "Ok" });
    } else {
      await displayError({
        title: "Error adding new device",
        message: "Unable to register new WebAuthn Device.",
        detail: result.message,
        primaryButton: "Ok",
      });
    }
    return { tag: "canceled" };
  }

  const alias = await inferPasskeyAlias({
    authenticatorType: result.getAuthenticatorAttachment(),
    userAgent: navigator.userAgent,
    uaParser,
    aaguid: result.aaguid,
  });

  // Finally, we submit it to the canister
  const device: Omit<DeviceData, "origin"> & { credential_id: [CredentialId] } =
    {
      alias: alias,
      protection: { unprotected: null },
      pubkey: Array.from(new Uint8Array(result.getPublicKey().toDer())),
      key_type: authenticatorAttachmentToKeyType(
        result.getAuthenticatorAttachment(),
      ),
      purpose: { authentication: null },
      credential_id: [Array.from(new Uint8Array(result.rawId))],
      metadata: [],
    };
  const addResponse = await addTentativeDevice({
    userNumber,
    connection,
    device,
  });

  if ("tag" in addResponse) {
    addResponse satisfies { tag: "canceled" };
    return addResponse;
  }

  // If everything went well we can now ask the user to authenticate on an existing device
  // and enter a verification code
  const verificationCodeResult = await showVerificationCode(
    userNumber,
    connection,
    device.alias,
    addResponse.added_tentatively,
    device.credential_id[0],
  );

  if (verificationCodeResult === "canceled") {
    return { tag: "canceled" };
  }

  verificationCodeResult satisfies "ok";

  await addDeviceSuccess({
    userNumber,
    deviceAlias: alias,
    stepper: tentativeDeviceStepper({ step: "success" }),
  });
  return { tag: "deviceAdded" };
};

/** Create new WebAuthn credentials */
const createDevice = async ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: Connection;
}): Promise<WebAuthnIdentity | Error> => {
  const existingAuthenticators =
    await connection.lookupAuthenticators(userNumber);
  try {
    return await WebAuthnIdentity.create({
      publicKey: creationOptions(existingAuthenticators),
    });
  } catch (error: unknown) {
    if (error instanceof Error) {
      return error;
    } else {
      return new Error(unknownToString(error, "unknown error"));
    }
  }
};

type AddDeviceSuccess = Extract<
  AddTentativeDeviceResponse,
  { added_tentatively: unknown }
>;

/** Add the device tentatively to the canister */
export const addTentativeDevice = async ({
  userNumber,
  connection,
  device,
}: {
  userNumber: bigint;
  connection: Connection;
  device: Omit<DeviceData, "origin">;
}): Promise<AddDeviceSuccess | { tag: "canceled" }> => {
  // Try to add the device tentatively, retrying if necessary
  for (;;) {
    const result = await withLoader(() =>
      connection.addTentativeDevice(userNumber, device),
    );

    if ("another_device_tentatively_added" in result) {
      // User already added a device so we show an error and abort
      await displayError({
        title: "Tentative Device Already Exists",
        message:
          'The "add device" process was already started for another device. If you want to add this device instead, log in using an existing device and restart the "add device" process.',
        primaryButton: "Ok",
      });
      return { tag: "canceled" };
    }

    if ("device_registration_mode_off" in result) {
      // User hasn't started the "add device" flow, so we offer to enable it and retry, or cancel
      const res = await deviceRegistrationDisabledInfo(userNumber);
      if (res === "canceled") {
        return { tag: "canceled" };
      }

      if (res === "retry") {
        continue;
      }

      // We should never get here, but just in case we retry
      unreachableLax(res);
      continue;
    }

    if ("added_tentatively" in result) {
      return result;
    }

    // We should never get here, but just in case we show an error
    unreachable(result);
    break;
  }
};
