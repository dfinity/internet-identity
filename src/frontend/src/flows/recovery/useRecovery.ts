import { DeviceData } from "../../../generated/internet_identity_types";
import { displayError } from "../../components/displayError";
import { IIConnection, LoginResult } from "../../utils/iiConnection";
import { apiResultToLoginResult } from "../loginUnknown";
import { renderManage } from "../manage";
import { promptUserNumber } from "../promptUserNumber";
import { inputSeedPhrase } from "./inputSeedPhrase";
import { pickRecoveryDevice } from "./pickRecoveryDevice";

const isRecovery = (device: DeviceData): boolean => {
  // TODO
  return true;
};

const wantsSeedPhrase = (device: DeviceData): boolean => {
  // TODO
  return true;
};

export const useRecovery = async (): Promise<void> => {
  const userNumber = await promptUserNumber("Recover your Identity", null);
  const devices = await IIConnection.lookup(userNumber);
  const recoveryDevices = devices.filter(isRecovery);
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
      // TODO: Display a recovery specific error
      await displayError({ ...loginResult, primaryButton: "Try again" });
      return useRecovery();
    }
  }
};

const loginWithRecovery = async (
  userNumber: bigint,
  device: DeviceData
): Promise<LoginResult> => {
  // TODO: Would be nicer as a switch
  if (wantsSeedPhrase(device)) {
    const seedPhrase = await inputSeedPhrase();
    return IIConnection.fromSeedPhrase(seedPhrase);
  } else {
    return IIConnection.fromWebauthnDevices(userNumber, [device]);
  }
};
