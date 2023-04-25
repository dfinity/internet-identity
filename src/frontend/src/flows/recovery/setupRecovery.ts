import { DeviceData } from "$generated/internet_identity_types";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { fromMnemonicWithoutValidation } from "$src/crypto/ed25519";
import { generate } from "$src/crypto/mnemonic";
import {
  AuthenticatedConnection,
  creationOptions,
  IC_DERIVATION_PATH,
} from "$src/utils/iiConnection";
import { unknownToString, unreachable, unreachableLax } from "$src/utils/utils";
import { DerEncodedPublicKey, SignIdentity } from "@dfinity/agent";
import { WebAuthnIdentity } from "@dfinity/identity";
import type { ChooseRecoveryProps } from "./chooseRecoveryMechanism";
import { chooseRecoveryMechanism } from "./chooseRecoveryMechanism";
import { confirmSeedPhrase } from "./confirmSeedPhrase";
import { displaySeedPhrase } from "./displaySeedPhrase";

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
  // Retry until user explicitly cancels or until a device is added successfully
  for (;;) {
    // Fetch all devices, which are used when offering recovery options & when setting up
    // a recovery device
    const devices = await withLoader(() => connection.lookupAll(userNumber));
    const recoveryMechanism = await chooseRecoveryMechanism({
      devices,
      title,
      message,
      cancelText,
    });
    if (recoveryMechanism === "canceled") {
      return;
    }

    // For phrases, we kick start the phrase setup wizard
    if (recoveryMechanism === "seedPhrase") {
      const res = await setupPhrase(userNumber, connection);
      if (res === "ok" || res === "canceled") {
        return;
      }

      if (res === "error") {
        await displayError({
          title: "Failed to set up recovery",
          message: "We failed to set up recovery for this Identity Anchor.",
          primaryButton: "Retry",
        });
        continue;
      }

      return unreachable(res, "Unexpected return value when creating phrase");
    }

    // For recovery keys, we retry until a key was successfully added (or the user explicitely cancels
    // when choosing a recovery mechanism upon retry)
    if (recoveryMechanism === "securityKey") {
      const res = await setupKey({ connection, devices });

      if (res === "ok") {
        return;
      }

      if ("error" in res) {
        await displayError({
          title: "Authentication failure",
          message:
            "Failed to set up a security key as your recovery method. If you don't have an additional security key you can use a recovery phrase instead.",
          detail: unknownToString(res.error, "Unknown error"),
          primaryButton: "Continue",
        });
        continue;
      }

      // exhaust return values
      return unreachable(res, "Unexpected return value when adding recovery");
    }

    return unreachable(
      recoveryMechanism,
      "Unexpected return value when choosing recovery"
    );
  }
};

// Set up a recovery device
export const setupKey = async ({
  devices: devices_,
  connection,
}: {
  // When provided, use these devices for exclusion (webauthn) instead of looking up devices
  // (avoids a request saves a couple seconds when used)
  devices?: Omit<DeviceData, "alias">[];
  connection: AuthenticatedConnection;
}): Promise<"ok" | { error: unknown }> => {
  const name = "Recovery key";
  try {
    // Create the WebAuthn credentials and upload them to the canister
    await withLoader(async () => {
      const devices =
        devices_ ?? (await connection.lookupAll(connection.userNumber));
      const recoverIdentity = await WebAuthnIdentity.create({
        publicKey: creationOptions(devices, "cross-platform"),
      });

      await connection.add(
        name,
        { cross_platform: null },
        { recovery: null },
        recoverIdentity.getPublicKey().toDer(),
        { unprotected: null },
        recoverIdentity.rawId
      );
    });
  } catch (error: unknown) {
    return { error };
  }

  return "ok";
};

// Set up a recovery phrase
export const setupPhrase = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<"ok" | "error" | "canceled"> => {
  const res = await phraseWizard({
    userNumber,
    operation: "create",
    uploadPhrase: (pubkey) =>
      withLoader(() =>
        connection.add(
          "Recovery phrase",
          { seed_phrase: null },
          { recovery: null },
          pubkey,
          { unprotected: null }
        )
      ),
  });

  if ("ok" in res) {
    return "ok";
  } else if ("error" in res) {
    return "error";
  } else {
    res satisfies { canceled: void };
    return "canceled";
  }
};

// Set up a recovery phrase
export const phraseWizard = async ({
  userNumber,
  operation,
  uploadPhrase,
}: {
  userNumber: bigint;
  operation: "create" | "reset";
  uploadPhrase: (pubkey: DerEncodedPublicKey) => Promise<void>;
}): Promise<{ ok: SignIdentity } | { error: unknown } | { canceled: void }> => {
  const seedPhrase = generate().trim();
  const recoverIdentity = await fromMnemonicWithoutValidation(
    seedPhrase,
    IC_DERIVATION_PATH
  );

  const phrase = userNumber.toString(10) + " " + seedPhrase;
  const res = await displayAndConfirmPhrase({ phrase, operation });

  if (res === "canceled") {
    return { canceled: undefined };
  }

  res satisfies "confirmed";

  try {
    const pubkey = recoverIdentity.getPublicKey().toDer();
    await withLoader(() => uploadPhrase(pubkey));
    return { ok: recoverIdentity };
  } catch (error: unknown) {
    return { error };
  }
};

// Show the new recovery phrase and ask for confirmation
export const displayAndConfirmPhrase = async ({
  operation,
  phrase,
}: {
  operation: "create" | "reset";
  phrase: string;
}): Promise<"confirmed" | "canceled"> => {
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
      return unreachable(displayResult, "unexpected return value");
    }

    const result = await confirmSeedPhrase({ phrase });
    // User has confirmed, so break out of the loop
    if (result === "confirmed") {
      return "confirmed";
    }

    // User has clicked the back button, so we retry
    if (result === "back") {
      continue;
    }

    unreachableLax(result);
  }
};
