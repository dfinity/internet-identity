import { DerEncodedPublicKey } from "@dfinity/agent";
import {
  bufferEqual,
  AuthenticatedConnection,
  Connection,
} from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { unreachable, unreachableLax } from "../../utils/utils";
import { DeviceData } from "../../../generated/internet_identity_types";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";
import { displayAndConfirmPhrase } from "../recovery/setupRecovery";
import {
  isRecoveryDevice,
  isProtected,
  RecoveryPhrase,
} from "../../utils/recoveryDevice";
import { generate } from "../../crypto/mnemonic";
import { fromMnemonicWithoutValidation } from "../../crypto/ed25519";
import { IC_DERIVATION_PATH } from "../../utils/iiConnection";

/* Remove the device and return */
export const deleteDevice = async ({
  connection,
  device,
  reload,
}: {
  connection: AuthenticatedConnection;
  device: DeviceData;
  reload: () => void;
}) => {
  const pubKey: DerEncodedPublicKey = new Uint8Array(device.pubkey)
    .buffer as DerEncodedPublicKey;
  const sameDevice = bufferEqual(
    connection.identity.getPublicKey().toDer(),
    pubKey
  );

  // Different confirmation based on the device
  const confirmationPrompt = [];
  if (isRecoveryDevice(device)) {
    confirmationPrompt.push("Remove your Recovery Device");
    confirmationPrompt.push(
      "Are you sure you want to remove your recovery device? You will no longer be able to use it to recover your account."
    );
  } else {
    confirmationPrompt.push(
      `Do you really want to remove the device "${device.alias}"?`
    );
  }
  if (sameDevice) {
    confirmationPrompt.push(
      "This will remove your current device and you will be logged out."
    );
  }
  const shouldProceed = confirm(confirmationPrompt.join("\n\n"));
  if (!shouldProceed) {
    return;
  }

  await withLoader(async () => {
    await connection.remove(device.pubkey);
  });

  if (sameDevice) {
    // clear anchor and reload the page.
    // do not call "reload", otherwise the management page will try to reload the list of devices which will cause an error
    localStorage.clear();
    location.reload();
    return;
  } else {
    reload();
  }
};

/* Reset the device and return to caller */
export const resetPhrase = async ({
  userNumber,
  connection,
  device,
  reload,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  device: DeviceData & RecoveryPhrase;
  reload: (connection?: AuthenticatedConnection) => void;
}) => {
  const confirmed = confirm(
    "Reset your Recovery Phrase\n\nWas your recovery phrase compromised? Delete your recovery phrase and generate a new one."
  );
  if (!confirmed) {
    return;
  }

  // Create a new recovery phrase
  const recoveryPhrase = generate().trim();
  const recoverIdentity = await fromMnemonicWithoutValidation(
    recoveryPhrase,
    IC_DERIVATION_PATH
  );

  // Figure out if we need a new connection
  // NOTE: we create this _before_ replacing the phrase, just in case something
  // goes wrong it goes wrong before we've replaced the phrase.
  let nextConnection: AuthenticatedConnection | undefined;
  const sameDevice = bufferEqual(
    connection.identity.getPublicKey().toDer(),
    new Uint8Array(device.pubkey).buffer as DerEncodedPublicKey
  );
  if (sameDevice) {
    nextConnection = await connection.fromIdentity(userNumber, recoverIdentity);
  }

  // The connection used in the replace op
  // (if the phrase is protected, this prompts for the phrase and builds a new connection)
  const opConnection = isProtected(device)
    ? await deviceConnection(
        connection,
        userNumber,
        device,
        "Please input your recovery phrase to reset it."
      )
    : connection;
  if (opConnection === null) {
    // User aborted, just return
    reload();
    return;
  }

  // Save the old pubkey (used as index for replace)
  const oldKey = device.pubkey;
  device.pubkey = Array.from(
    new Uint8Array(recoverIdentity.getPublicKey().toDer())
  );

  try {
    const phrase = userNumber.toString(10) + " " + recoveryPhrase;

    const res = await displayAndConfirmPhrase({ phrase, operation: "reset" });

    if (res === "confirmed") {
      await withLoader(() => opConnection.replace(oldKey, device));
    } else if (res !== "canceled") {
      unreachableLax(res);
    }
  } catch (e: unknown) {
    await displayError({
      title: "Could not reset recovery phrase",
      message: "An unexpected error occurred",
      detail: e instanceof Error ? e.toString() : "unknown error",
      primaryButton: "Ok",
    });
  }

  reload(nextConnection);
};

/* Protect the device and re-render the device settings (with the updated device) */
export const protectDevice = async ({
  userNumber,
  connection,
  device,
  reload,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  device: DeviceData & RecoveryPhrase;
  reload: () => void;
}) => {
  const confirmed = confirm(
    "Lock your Recovery Phrase\n\nIf you lock your recovery phrase, you will not be able to reset it if you lose access to it or cannot remember it."
  );
  if (!confirmed) {
    return;
  }

  device.protection = { protected: null };

  // NOTE: we do _not_ need to be authenticated with the device in order to protect it,
  // but we do it to make sure one last time that the user can actually successfully authenticate
  // with the device.
  const newConnection = await deviceConnection(
    connection,
    userNumber,
    device,
    "Please input your recovery phrase to lock it."
  );

  await withLoader(async () => {
    // if null then user canceled so we just redraw the manage page
    if (newConnection == null) {
      await reload();
      return;
    }

    await newConnection.update(device);
  });
  reload();
};

/* Unprotect the device and re-render the device settings (with the updated device) */
export const unprotectDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData & RecoveryPhrase,
  back: () => void
) => {
  const confirmed = confirm(
    "Unlock your Recovery Phrase\n\nIf you unlock your recovery phrase, you will be able to reset your recovery phrase without re-entering the current phrase."
  );
  if (!confirmed) {
    return;
  }

  device.protection = { unprotected: null };

  // NOTE: we do need to be authenticated with the device in order to unprotect it
  const newConnection = await deviceConnection(
    connection,
    userNumber,
    device,
    "Please input your recovery phrase to unlock it."
  );

  await withLoader(async () => {
    // if null then user canceled so we just redraw the manage page
    if (newConnection == null) {
      await back();
      return;
    }

    await newConnection.update(device);
  });
  back();
};

// Get a connection that's authenticated with the given device
// NOTE: this expects a recovery phrase device
const deviceConnection = async (
  connection: Connection,
  userNumber: bigint,
  device: DeviceData & RecoveryPhrase,
  recoveryPhraseMessage: string
): Promise<AuthenticatedConnection | null> => {
  try {
    const loginResult = await phraseRecoveryPage(
      userNumber,
      connection,
      device,
      undefined,
      recoveryPhraseMessage
    );
    switch (loginResult.tag) {
      case "ok":
        return loginResult.connection;
      case "canceled":
        return null;
      default:
        unreachable(loginResult);
        break;
    }
  } catch (error: unknown) {
    await displayError({
      title: "Could not modify device",
      message:
        "An unexpected error occurred when trying to read recovery phrase for device modification.",
      detail: error instanceof Error ? error.toString() : "unknown error",
      primaryButton: "Ok",
    });
    return null;
  }
};
