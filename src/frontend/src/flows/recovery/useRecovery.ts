import { promptDeviceAlias } from "$src/components/alias";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { setAnchorUsed } from "$src/storage";
import { authenticatorAttachmentToKeyType } from "$src/utils/authenticatorAttachment";
import { LoginFlowResult } from "$src/utils/flowResult";
import { AuthenticatedConnection, Connection } from "$src/utils/iiConnection";
import { unknownToString, unreachableLax } from "$src/utils/utils";
import { constructIdentity } from "$src/utils/webAuthn";
import {
  displayCancelError,
  displayDuplicateDeviceError,
  isWebAuthnCancel,
  isWebAuthnDuplicateDevice,
} from "$src/utils/webAuthnErrorUtils";
import { html } from "lit-html";
import { forgotNumber } from "./forgotNumber";
import { promptRecovery } from "./promptRecovery";
import { recoverWithDevice } from "./recoverWith/device";
import { recoverWithPhrase } from "./recoverWith/phrase";

export const useRecovery = async (
  connection: Connection
): Promise<LoginFlowResult> => {
  const res = await promptRecovery();

  if (res === "forgotten") {
    const cancel = await forgotNumber();
    cancel satisfies "cancel";
    return { tag: "canceled" };
  }

  if (res === "cancel") {
    return { tag: "canceled" };
  }

  res satisfies "phrase" | "device";

  const op =
    res === "phrase"
      ? recoverWithPhrase({
          connection,
          message: html`Your recovery phrase includes your Internet Identity
          number and a unique combination of 24 words that represent your
          private key`,
        })
      : recoverWithDevice({ connection });

  const flowResult = await op;

  if (flowResult.tag === "ok") {
    await postRecoveryFlow(flowResult);
  }

  return flowResult;
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
    if (isWebAuthnDuplicateDevice(error)) {
      await displayDuplicateDeviceError({ primaryButton: "Ok" });
    } else if (isWebAuthnCancel(error)) {
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
