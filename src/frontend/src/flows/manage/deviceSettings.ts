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
  RecoveryDevice,
} from "../../utils/recoveryDevice";

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

  // Whether the device can be protected or not
  if (shouldOfferToProtect(device)) {
    settings.push({
      label: "protect",
      fn: () => protectDevice({ userNumber, connection, device, reload }),
    });
  }

  // Whether the device can be unprotected or not
  if (shouldOfferToUnprotect(device)) {
    settings.push({
      label: "unprotect",
      fn: () =>
        unprotectDevice(userNumber, connection, device, isOnlyDevice, reload),
    });
  }

  // If this is _not_ the only device, then we allow removing it
  if (!isOnlyDevice) {
    settings.push({
      label: "remove",
      fn: () => deleteDevice({ userNumber, connection, device, reload }),
    });
  }

  return settings;
};

// We offer to protect unprotected recovery phrases only, although in the
// future we may offer to protect all devices
const shouldOfferToProtect = (
  device: DeviceData
): device is RecoveryDevice & DeviceData =>
  isRecoveryPhrase(device) && !isProtected(device);

// We offer to unprotect protected recovery phrases only
const shouldOfferToUnprotect = (
  device: DeviceData
): device is RecoveryDevice & DeviceData =>
  isRecoveryPhrase(device) && isProtected(device);

// Get a connection that's authenticated with the given device
// NOTE: this expects a recovery phrase device
const deviceConnection = async (
  connection: Connection,
  userNumber: bigint,
  device: DeviceData & RecoveryDevice,
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
      title: "Could not delete device",
      message:
        "An unexpected error occurred when trying to read recovery phrase for device deletion.",
      detail: error instanceof Error ? error.toString() : "unknown error",
      primaryButton: "Ok",
    });
    return null;
  }
};

/* Remove the device and return */
const deleteDevice = async ({
  userNumber,
  connection,
  device,
  reload,
}: {
  userNumber: bigint;
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

  // If the device is protected then we need to be authenticated with the device to remove it
  // NOTE: the user may be authenticated with the device already, but for consistency we still ask them to input their recovery phrase
  const removalConnection =
    isRecoveryDevice(device) && isProtected(device)
      ? await deviceConnection(
          connection,
          userNumber,
          device,
          "Please input your recovery phrase to remove it."
        )
      : connection;

  await withLoader(async () => {
    // if null then user canceled so we just redraw the manage page
    if (removalConnection == null) {
      await reload();
      return;
    }
    await removalConnection.remove(device.pubkey);
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

/* Protect the device and re-render the device settings (with the updated device) */
const protectDevice = async ({
  userNumber,
  connection,
  device,
  reload,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  device: DeviceData & RecoveryDevice;
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
  device: DeviceData & RecoveryDevice,
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
