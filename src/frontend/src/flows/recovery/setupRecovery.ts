import { WebAuthnIdentity } from "@dfinity/identity";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { fromMnemonicWithoutValidation } from "../../crypto/ed25519";
import { generate } from "../../crypto/mnemonic";
import {
  creationOptions,
  IC_DERIVATION_PATH,
  AuthenticatedConnection,
} from "../../utils/iiConnection";
import { unknownToString } from "../../utils/utils";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { displaySeedPhrase } from "./displaySeedPhrase";

export const setupRecovery = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  const devices = await connection.lookupAll(userNumber);
  const recoveryMechanism = await chooseRecoveryMechanism(devices);
  if (recoveryMechanism === null) {
    return;
  }

  try {
    switch (recoveryMechanism) {
      case "securityKey": {
        const name = "Recovery key";
        let recoverIdentity: WebAuthnIdentity;
        try {
          recoverIdentity = await WebAuthnIdentity.create({
            publicKey: creationOptions(devices, "cross-platform"),
          });
        } catch (err: unknown) {
          await displayError({
            title: "Authentication failure",
            message:
              "Failed to set up a security key as your recovery method. If you don't have an additional security key you can use a recovery phrase instead.",
            detail: unknownToString(err, "Unknown error"),
            primaryButton: "Try a different method",
          });
          return setupRecovery(userNumber, connection);
        }

        return await withLoader(() =>
          connection.add(
            name,
            { cross_platform: null },
            { recovery: null },
            recoverIdentity.getPublicKey().toDer(),
            { unprotected: null },
            recoverIdentity.rawId
          )
        );
      }
      case "seedPhrase": {
        await setupPhrase(userNumber, connection);
        break;
      }
    }
  } catch (err: unknown) {
    await displayError({
      title: "Failed to set up recovery",
      message: "We failed to set up recovery for this Identity Anchor.",
      detail: unknownToString(err, "Unkwnown error"),
      primaryButton: "Continue",
    });
  }
};

export const setupPhrase = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
) => {
  const name = "Recovery phrase";
  const seedPhrase = generate().trim();
  const recoverIdentity = await fromMnemonicWithoutValidation(
    seedPhrase,
    IC_DERIVATION_PATH
  );
  await withLoader(() =>
    connection.add(
      name,
      { seed_phrase: null },
      { recovery: null },
      recoverIdentity.getPublicKey().toDer(),
      { unprotected: null }
    )
  );
  await displaySeedPhrase(userNumber.toString(10) + " " + seedPhrase);
};
