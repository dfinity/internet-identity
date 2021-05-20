import { IIConnection } from "../../utils/iiConnection";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";

export const setupRecovery = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const recoveryMechanism = await chooseRecoveryMechanism();
  if (recoveryMechanism === null) {
    return;
  }

  switch (recoveryMechanism) {
    case "securityKey": {
      const name = "Recovery key";
      // 2 a) Security key: Prompt for user touch, generate credential, add it as a recovery device
      return;
    }
    case "seedPhrase": {
      const name = "Recovery phrase";
      // 2 b) Seed phrase: Generate Seed phrase, display it to the user, (make them print it back?), add it as a recovery device
      return;
    }
  }
};
