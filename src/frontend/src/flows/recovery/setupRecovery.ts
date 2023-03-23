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
import {
  unknownToString,
  unreachable,
  unreachableLax,
} from "../../utils/utils";
import type { ChooseRecoveryProps } from "./chooseRecoveryMechanism";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { displaySeedPhrase } from "./displaySeedPhrase";
import { confirmSeedPhrase } from "./confirmSeedPhrase";

export const setupRecovery = async ({
  userNumber,
  connection,
  title,
  message,
  cancelText,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
} & ChooseRecoveryProps): Promise<void> => {
  const devices = await connection.lookupAll(userNumber);
  const recoveryMechanism = await chooseRecoveryMechanism({
    devices,
    title,
    message,
    cancelText,
  });
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
          return setupRecovery({
            userNumber,
            connection,
            title,
            message,
            cancelText,
          });
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

export const displayAndConfirmPhrase = async ({
  operation,
  phrase,
}: {
  operation: "create" | "reset";
  phrase: string;
}): Promise<"ok" | "error" | "canceled"> => {
  // Loop until the user has confirmed the phrase
  for (;;) {
    const displayResult = await displaySeedPhrase({
      seedPhrase: phrase,
      operation,
    });
    // User has canceled, so we return
    if (displayResult === "canceled") {
      return "canceled";
    }

    if (displayResult !== "ok") {
      // According to typescript, should never happen
      unreachable(displayResult, "unexpected return value");
      return "error";
    }

    const result = await confirmSeedPhrase({ phrase });
    // User has confirmed, so break out of the loop
    if (result === "confirmed") {
      return "ok";
    }

    // User has clicked the back button, so we retry
    if (result === "back") {
      continue;
    }

    unreachableLax(result);
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

  const phrase = userNumber.toString(10) + " " + seedPhrase;

  const res = await displayAndConfirmPhrase({ phrase, operation: "create" });

  if (res === "error" || res === "canceled") {
    return;
  }

  if (res !== "ok") {
    unreachableLax(res);
    return;
  }

  await withLoader(() =>
    connection.add(
      name,
      { seed_phrase: null },
      { recovery: null },
      recoverIdentity.getPublicKey().toDer(),
      { unprotected: null }
    )
  );
};
