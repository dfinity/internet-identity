import { html } from "lit-html";
import { displayError } from "../../components/displayError";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { LoginFlowResult } from "../../utils/flowResult";
import { promptUserNumber } from "../../components/promptUserNumber";
import { promptDeviceAlias } from "../../components/alias";
import { recoverWithPhrase } from "./recoverWith/phrase";
import { deviceRecoveryPage } from "./recoverWith/device";
import { pickRecoveryDevice } from "./pickRecoveryDevice";
import { isRecoveryPhrase } from "../../utils/recoveryDevice";
import { setAnchorUsed } from "../../utils/userNumber";
import { unknownToString, unreachableLax } from "../../utils/utils";
import { constructIdentity } from "../register/construct";
import { authenticatorAttachmentToKeyType } from "../../utils/authenticatorAttachment";

export const useRecovery = async (
  connection: Connection,
  userNumber?: bigint
): Promise<LoginFlowResult> => {
  if (userNumber !== undefined) {
    return runRecovery(userNumber, connection);
  } else {
    const pUserNumber = await promptUserNumber({
      title: "Recover Identity Anchor",
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
      message: `You do not have any recovery devices configured for anchor ${userNumber}. Did you mean to authenticate with one of your devices instead?`,
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
        userNumber,
        connection,
        device,
        message: html`Type your recovery phrase below to access your anchor
          <strong class="t-strong">${userNumber}</strong>`,
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
    newDevice = await constructIdentity({
      devices: async () => {
        return (await connection.getAnchorInfo()).devices;
      },
      message: "Enrolling device...",
    });
  } catch (error: unknown) {
    await displayError({
      title: "Could not enroll device",
      message:
        "Something went wrong when we were trying to remember this device. Could you try again?",
      detail:
        "Could not create credentials: " +
        unknownToString(error, "unknown error"),
      primaryButton: "Ok",
    });
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
        "The device could not be added to the anchor: " +
        unknownToString(error, "unknown error"),
      primaryButton: "Ok",
    });
    return "error";
  }

  setAnchorUsed(userNumber);
  return "enrolled";
};
