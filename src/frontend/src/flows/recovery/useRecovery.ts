import { promptDeviceAlias } from "$src/components/alias";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { promptUserNumber } from "$src/components/promptUserNumber";
import { authenticatorAttachmentToKeyType } from "$src/utils/authenticatorAttachment";
import { LoginFlowResult } from "$src/utils/flowResult";
import { AuthenticatedConnection, Connection } from "$src/utils/iiConnection";
import { isRecoveryPhrase } from "$src/utils/recoveryDevice";
import { setAnchorUsed } from "$src/utils/userNumber";
import { unknownToString, unreachableLax } from "$src/utils/utils";
import { constructIdentity } from "$src/utils/webAuthn";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isCancel,
  isDuplicateDeviceError,
} from "$src/utils/webAuthnErrorUtils";
import { nonNullish } from "@dfinity/utils";
import { html } from "lit-html";
import { pickRecoveryDevice } from "./pickRecoveryDevice";
import { deviceRecoveryPage } from "./recoverWith/device";
import { recoverWithPhrase } from "./recoverWith/phrase";

export const useRecovery = async (
  connection: Connection,
  userNumber?: bigint
): Promise<LoginFlowResult> => {
  if (nonNullish(userNumber)) {
    return runRecovery(userNumber, connection);
  } else {
    const pUserNumber = await promptUserNumber({
      title: "Recover Internet Identity",
    });
    if (pUserNumber !== "canceled") {
      return runRecovery(pUserNumber, connection);
    } else {
      return window.location.reload() as never;
    }
  }
};

const runRecovery = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  const recoveryDevices = await connection.lookupRecovery(userNumber);
  if (recoveryDevices.length === 0) {
    await displayError({
      title: "Failed to recover",
      message: `You do not have any recovery devices configured for Internet Identity ${userNumber}. Did you mean to authenticate with one of your devices instead?`,
      primaryButton: "Go back",
    });
    return window.location.reload() as never;
  }

  const device =
    recoveryDevices.length === 1
      ? recoveryDevices[0]
      : await pickRecoveryDevice(recoveryDevices);

  const res = isRecoveryPhrase(device)
    ? await recoverWithPhrase({
        connection,
        message: html`Type your recovery phrase below to access your Internet
          Identity <strong class="t-strong">${userNumber}</strong>`,
      })
    : await deviceRecoveryPage(userNumber, connection, device);

  // If res is null, the user canceled the flow, so we go back to the main page.
  if (res.tag === "canceled") {
    return window.location.reload() as never;
  }

  // Wrap up recovery (notify of successful recovery, potentially add device)
  await postRecoveryFlow({ userNumber, connection: res.connection });

  return res;
};

// Show the user that the recovery was successful and offer to enroll a new device
const postRecoveryFlow = async ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
}) => {
  // The original title & message which mention a "successful recovery"; subsequently when retrying the
  // messages don't need to mention the recovery
  let title = "Successful Recovery";
  let message =
    "Remember this device to avoid losing access to your account again. What device are you using?";

  // Offer to enroll the device repeatedly, until the user explicitly skips
  for (;;) {
    const deviceAlias = await promptDeviceAlias({
      title: title,
      message: message,
      cancelText: "Skip",
    });

    // The user clicked "skip"
    if (deviceAlias === null) {
      break;
    }

    const enrollResult = await enrollAuthenticator({
      connection: connection,
      userNumber: userNumber,
      deviceAlias,
    });

    // The device was successfully enrolled, so we break out of the loop
    if (enrollResult === "enrolled") {
      break;
    }

    // There was an error enrolling the device, so we retry
    if (enrollResult === "error") {
      title = "Remember this Device";
      message = "What device are you using?";
      continue;
    }

    // we should never get here, but in case the unexpected happens, we break out of the loop
    unreachableLax(enrollResult);
    break;
  }
};

// Offer to enroll a new device
const enrollAuthenticator = async ({
  connection,
  userNumber,
  deviceAlias,
}: {
  connection: AuthenticatedConnection;
  userNumber: bigint;
  deviceAlias: string;
}): Promise<"enrolled" | "error"> => {
  let newDevice;
  try {
    newDevice = await withLoader(() =>
      constructIdentity({
        devices: async () => {
          return (await connection.getAnchorInfo()).devices;
        },
      })
    );
  } catch (error: unknown) {
    if (isDuplicateDeviceError(error)) {
      await displayDuplicateDeviceError({ primaryButton: "Ok" });
    } else if (isCancel(error)) {
      await displayCancelError({ primaryButton: "Ok" });
    } else {
      await displayError({
        title: "Could not enroll device",
        message:
          "Something went wrong when we were trying to remember this device. Could you try again?",
        detail:
          "Could not create credentials: " +
          unknownToString(error, "unknown error"),
        primaryButton: "Ok",
      });
    }
    return "error";
  }

  // XXX: Bit of a hack here: `constructIdentity` starts a loader but doesn't
  // display anything else if everything goes well, meaning the loader is still
  // running. Instead of starting a new loader explicitly (which would cause
  // flicker) we simply use `constructIdentity`'s.
  try {
    await connection.add(
      deviceAlias,
      authenticatorAttachmentToKeyType(newDevice.getAuthenticatorAttachment()),
      { authentication: null },
      newDevice.getPublicKey().toDer(),
      { unprotected: null },
      newDevice.rawId
    );
  } catch (error: unknown) {
    await displayError({
      title: "Failed to Remember Device",
      message:
        "Something went wrong when we were trying to remember this device. Could you try again?",
      detail:
        "The Passkey could not be added to the Internet Identity: " +
        unknownToString(error, "unknown error"),
      primaryButton: "Ok",
    });
    return "error";
  }

  setAnchorUsed(userNumber);
  return "enrolled";
};
