import { displayError } from "../../components/displayError";
import { Connection } from "../../utils/iiConnection";
import { renderManage } from "../manage";
import { promptUserNumber } from "../../components/promptUserNumber";
import { phraseRecoveryPage } from "./recoverWith/phrase";
import { deviceRecoveryPage } from "./recoverWith/device";
import { pickRecoveryDevice } from "./pickRecoveryDevice";

export const useRecovery = async (
  connection: Connection,
  userNumber?: bigint
): Promise<void> => {
  if (userNumber !== undefined) {
    return runRecovery(userNumber, connection);
  } else {
    const pUserNumber = await promptUserNumber({
      title: "Recover Identity Anchor",
    });
    if (pUserNumber !== "canceled") {
      return runRecovery(pUserNumber, connection);
    } else {
      return window.location.reload();
    }
  }
};

const runRecovery = async (
  userNumber: bigint,
  connection: Connection
): Promise<void> => {
  const recoveryDevices = await connection.lookupRecovery(userNumber);
  if (recoveryDevices.length === 0) {
    await displayError({
      title: "Failed to recover",
      message: `You do not have any recovery devices configured for anchor ${userNumber}. Did you mean to authenticate with one of your devices instead?`,
      primaryButton: "Go back",
    });
    return window.location.reload();
  }

  const device =
    recoveryDevices.length === 1
      ? recoveryDevices[0]
      : await pickRecoveryDevice(recoveryDevices);

  const res =
    "seed_phrase" in device.key_type
      ? await phraseRecoveryPage(userNumber, connection, device)
      : await deviceRecoveryPage(userNumber, connection, device);

  // If res is null, the user canceled the flow, so we go back to the main page.
  if (res.tag === "canceled") {
    return window.location.reload();
  }

  renderManage(res.userNumber, res.connection);
};
