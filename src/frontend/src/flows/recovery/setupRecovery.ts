import { WebAuthnIdentity } from "@dfinity/identity";
import { withLoader } from "../../components/loader";
import { fromMnemonicWithoutValidation } from "../../crypto/ed25519";
import { generate } from "../../crypto/mnemonic";
import {
  creationOptions,
  IC_DERIVATION_PATH,
  IIConnection,
} from "../../utils/iiConnection";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { displaySeedPhrase } from "./displaySeedPhrase";

export const setupRecovery = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const devices = await IIConnection.lookupAll(userNumber);
  const recoveryMechanism = await chooseRecoveryMechanism(devices);
  if (recoveryMechanism === null) {
    return;
  }

  switch (recoveryMechanism) {
    case "securityKey": {
      const name = "Recovery key";
      const recoverIdentity = await WebAuthnIdentity.create({
        publicKey: creationOptions(devices, "cross-platform"),
      });
      return await withLoader(() =>
        connection.add(
          userNumber,
          name,
          { cross_platform: null },
          { recovery: null },
          recoverIdentity.getPublicKey().toDer(),
          recoverIdentity.rawId
        )
      );
    }
    case "seedPhrase": {
      const name = "Recovery phrase";
      const seedPhrase = generate().trim();
      await displaySeedPhrase(seedPhrase);
      const recoverIdentity = await fromMnemonicWithoutValidation(
        seedPhrase,
        IC_DERIVATION_PATH
      );
      await withLoader(() =>
        connection.add(
          userNumber,
          name,
          { seed_phrase: null },
          { recovery: null },
          recoverIdentity.getPublicKey().toDer()
        )
      );
    }
  }
};
