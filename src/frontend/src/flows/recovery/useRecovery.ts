import { displayError } from "../../components/displayError";
import { IIConnection } from "../../utils/iiConnection";
import { hasOwnProperty } from "../../utils/utils";
import { renderManage } from "../manage";
import { promptUserNumber } from "../promptUserNumber";
import { phraseRecoveryPage } from "./recoverWith/phrase";
import { deviceRecoveryPage } from "./recoverWith/device";
import { pickRecoveryDevice } from "./pickRecoveryDevice";

export const useRecovery = async (userNumber?: bigint): Promise<void> => {
  if (userNumber !== undefined) {
    return runRecovery(userNumber);
  } else {
    const pUserNumber = await promptUserNumber("Recover Identity Anchor", null);
    if (pUserNumber !== null) {
      return runRecovery(pUserNumber);
    } else {
      return window.location.reload();
    }
  }
};

const runRecovery = async (userNumber: bigint): Promise<void> => {
  const recoveryDevices = await IIConnection.lookupRecovery(userNumber);
  if (recoveryDevices.length === 0) {
    await displayError({
      title: "Failed to recover",
      message:
        "You do not have any recovery devices configured. Did you mean to authenticate with one of your devices instead?",
      primaryButton: "Go back",
    });
    return window.location.reload();
  }

  const device =
    recoveryDevices.length === 1
      ? recoveryDevices[0]
      : await pickRecoveryDevice(recoveryDevices);

  const res = hasOwnProperty(device.key_type, "seed_phrase")
    ? await phraseRecoveryPage(userNumber, device)
    : await deviceRecoveryPage(userNumber, device);

  // If res is null, the user canceled the flow, so we go back to the main page.
  if (res.tag === "canceled") {
    return window.location.reload();
  }

  renderManage(res.userNumber, res.connection);
};
