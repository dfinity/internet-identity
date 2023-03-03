import { displayError } from "../../components/displayError";
import { Connection, AuthenticatedConnection } from "../../utils/iiConnection";
import { LoginFlowResult } from "../../utils/flowResult";
import { promptUserNumber } from "../../components/promptUserNumber";
import { promptDeviceAlias } from "../../components/alias";
import { phraseRecoveryPage } from "./recoverWith/phrase";
import { deviceRecoveryPage } from "./recoverWith/device";
import { pickRecoveryDevice } from "./pickRecoveryDevice";
import { isRecoveryPhrase } from "../../utils/recoveryDevice";
import { setAnchorUsed } from "../../utils/userNumber";
import { unknownToString } from "../../utils/utils";
import { constructIdentity } from "../register/construct";

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
    ? await phraseRecoveryPage(userNumber, connection, device)
    : await deviceRecoveryPage(userNumber, connection, device);

  // If res is null, the user canceled the flow, so we go back to the main page.
  if (res.tag === "canceled") {
    return window.location.reload() as never;
  }

  const deviceAlias = await promptDeviceAlias({
    title: "Remember this Device",
    message: "Recovery successful. What device are you using?",
    cancelText: "Skip",
  });
  if (deviceAlias !== null) {
    // Offer to enroll a new authenticator
    await enrollAuthenticator({
      connection: res.connection,
      userNumber: res.userNumber,
      deviceAlias,
    });
  }

  return res;
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
}): Promise<void> => {
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
        "Could not create credentials: " +
        unknownToString(error, "unknown error"),
      primaryButton: "Ok",
    });
    return;
  }

  // XXX: Bit of a hack here: `constructIdentity` starts a loader but doesn't
  // display anything else if everything goes well, meaning the loader is still
  // running. Instead of starting a new loader explicitly (which would cause
  // flicker) we simply use `constructIdentity`'s.
  try {
    await connection.add(
      deviceAlias,
      { unknown: null },
      { authentication: null },
      newDevice.getPublicKey().toDer(),
      { unprotected: null },
      newDevice.rawId
    );
  } catch (error: unknown) {
    await displayError({
      title: "Could not enroll device",
      message:
        "The device could not be added to the anchor: " +
        unknownToString(error, "unknown error"),
      primaryButton: "Ok",
    });
    return;
  }

  setAnchorUsed(userNumber);
};
