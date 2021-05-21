import { DeviceData } from "../../../generated/internet_identity_types";
import { displayError } from "../../components/displayError";
import { IIConnection, LoginResult } from "../../utils/iiConnection";
import { hasOwnProperty } from "../../utils/utils";
import { apiResultToLoginResult } from "../loginUnknown";
import { renderManage } from "../manage";
import { promptUserNumber } from "../promptUserNumber";
import { inputSeedPhrase } from "./inputSeedPhrase";
import { pickRecoveryDevice } from "./pickRecoveryDevice";

const wantsSeedPhrase = (device: DeviceData): boolean => {
  return hasOwnProperty(device.key_type, "seed_phrase");
};

export const useRecovery = async (): Promise<void> => {
  const userNumber = await promptUserNumber("Recover your Identity", null);
  const recoveryDevices = await IIConnection.lookupRecovery(userNumber);
  if (recoveryDevices.length === 0) {
    await displayError({
      title: "Failed to recover",
      message:
        "You do not have any recovery devices configured. Did you mean to login with one of your devices instead?",
      primaryButton: "Go back",
    });
    return window.location.reload();
  }

  const recoveryDevice =
    recoveryDevices.length === 1
      ? recoveryDevices[0]
      : await pickRecoveryDevice(recoveryDevices);

  const loginResult = apiResultToLoginResult(
    await loginWithRecovery(userNumber, recoveryDevice)
  );
  switch (loginResult.tag) {
    case "ok": {
      return renderManage(loginResult.userNumber, loginResult.connection);
    }
    case "err": {
      // TODO Display a recovery specific error
      await displayError({ ...loginResult, primaryButton: "Try again" });
      return useRecovery();
    }
  }
};

const loginWithRecovery = async (
  userNumber: bigint,
  device: DeviceData
): Promise<LoginResult> => {
  // TODO Make this a switch
  if (wantsSeedPhrase(device)) {
    const seedPhrase = await inputSeedPhrase();
    if (seedPhrase === null) {
      // TODO think about error handling here
      throw Error("Canceled seedphrase input");
    }
    return await IIConnection.fromSeedPhrase(userNumber, seedPhrase);
  } else {
    return IIConnection.fromWebauthnDevices(userNumber, [device]);
  }
};
