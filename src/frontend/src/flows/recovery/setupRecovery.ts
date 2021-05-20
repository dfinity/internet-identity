import { withLoader } from "../../components/loader";
import { fromMnemonic } from "../../crypto/ed25519";
import { generateMnemonic } from "../../crypto/mnemonic";
import { IC_DERIVATION_PATH, IIConnection } from "../../utils/iiConnection";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { displaySeedPhrase } from "./displaySeedPhrase";

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
      const seedPhrase = generateMnemonic();
      await displaySeedPhrase(seedPhrase);
      const recoverIdentity = await fromMnemonic(
        seedPhrase,
        IC_DERIVATION_PATH
      );
      await withLoader(() =>
        connection.add(userNumber, name, recoverIdentity.getPublicKey().toDer())
      );
    }
  }
};
