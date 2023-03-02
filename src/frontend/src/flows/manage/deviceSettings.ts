import { DerEncodedPublicKey } from "@dfinity/agent";
import {
  bufferEqual,
  AuthenticatedConnection,
  Connection,
} from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { DeviceData } from "../../../generated/internet_identity_types";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";
import {
  isRecoveryDevice,
  isRecoveryPhrase,
  isProtected,
  RecoveryPhrase,
} from "../../utils/recoveryDevice";
import { generate } from "../../crypto/mnemonic";
import { fromMnemonicWithoutValidation } from "../../crypto/ed25519";
import { IC_DERIVATION_PATH } from "../../utils/iiConnection";
import { displaySeedPhrase } from "../recovery/displaySeedPhrase";

// A particular device setting, e.g. remove, protect, etc
export type Setting = { label: string; fn: () => void };

// Generate possible settings based on the device
export const deviceSettings = ({
  userNumber,
  connection,
  device,
  isOnlyDevice,
  reload,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  device: DeviceData;
  isOnlyDevice: boolean;
  /* Reload the page after the new settings were applied */
  reload: () => void;
}): Setting[] => {
  const settings: Setting[] = [];

  // Whether the device can be (un)protected or not (only recovery phrases can be protected)
  if (isRecoveryPhrase(device)) {
    if (!isProtected(device)) {
      settings.push({
        label: "protect",
        fn: () => protectDevice({ userNumber, connection, device, reload }),
      });
    } else {
      settings.push({
        label: "unprotect",
        fn: () =>
          unprotectDevice(userNumber, connection, device, isOnlyDevice, reload),
      });
    }
  }

  // For recovery phrases, we only allow resetting, not removing
  if (isRecoveryPhrase(device)) {
    settings.push({
      label: "reset",
      fn: () => resetPhrase({ userNumber, connection, device, reload }),
    });
  } else {
    // For all other devices, we allow removing (unless it's the only device)
    if (!isOnlyDevice) {
      settings.push({
        label: "remove",
        fn: () => deleteDevice({ connection, device, reload }),
      });
    }
  }

  return settings;
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

/* Remove the device and return */
const deleteDevice = async ({
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

  const shouldProceed = sameDevice
    ? confirm(
        "This will remove your current device and you will be logged out."
      )
    : confirm(
        `Do you really want to remove the ${
          isRecoveryDevice(device) ? "" : "device "
        }"${device.alias}"?`
      );
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
const resetPhrase = async ({
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
    "Reset your recovery phrase\n\nWas your recovery phrase compromised? Delete your recovery phrase and generate a new one by confirming."
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

  // If the phrase is protected, prompt for the phrase
  const newConnection = isProtected(device)
    ? await deviceConnection(
        connection,
        userNumber,
        device,
        "Please input your recovery phrase to reset it."
      )
    : connection;

  if (newConnection === null) {
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
    await withLoader(() => newConnection.replace(oldKey, device));
    await displaySeedPhrase(userNumber.toString(10) + " " + recoveryPhrase);
  } catch (e: unknown) {
    await displayError({
      title: "Could not reset recovery phrase",
      message: "An unexpected error occurred",
      detail: e instanceof Error ? e.toString() : "unknown error",
      primaryButton: "Ok",
    });
  }

  reload();
};

/* Protect the device and re-render the device settings (with the updated device) */
const protectDevice = async ({
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
  device.protection = { protected: null };

  // NOTE: we do _not_ need to be authenticated with the device in order to protect it,
  // but we do it to make sure one last time that the user can actually successfully authenticate
  // with the device.
  const newConnection = await deviceConnection(
    connection,
    userNumber,
    device,
    "Please input your recovery phrase to protect it."
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

/* Protect the device and re-render the device settings (with the updated device) */
const unprotectDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData & RecoveryPhrase,
  isOnlyDevice: boolean,
  back: () => void
) => {
  device.protection = { unprotected: null };

  // NOTE: we do need to be authenticated with the device in order to unprotect it
  const newConnection = await deviceConnection(
    connection,
    userNumber,
    device,
    "Please input your recovery phrase to unprotect it."
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
