import { render, html } from "lit-html";
import { DerEncodedPublicKey } from "@dfinity/agent";
import {
  bufferEqual,
  AuthenticatedConnection,
  Connection,
} from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { footer } from "../../components/footer";
import { DeviceData } from "../../../generated/internet_identity_types";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";

// The "device settings" page where users can view information about a device,
// remove a device, make a recovery phrase protected, etc.

// Actual page content. We display options for protecting recovery phrases or
// deleting general devices.
const pageContent = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData,
  isOnlyDevice: boolean,
  back: () => void
) => html`
  <article id="deviceSettings" class="l-container c-card c-card--highlight">
    <h1 class="t-title">
      ${isRecovery(device) ? "" : "Device"} ${device.alias}
    </h1>

    <div class="l-stack">
      ${shouldOfferToProtect(device)
        ? html` <p class="t-paragraph">
              By making your recovery phrase protected, you will need to input
              your recovery phrase to delete it.
            </p>
            <button
              @click="${() =>
                protectDevice(
                  userNumber,
                  connection,
                  device,
                  isOnlyDevice,
                  back
                )}"
              data-action="protect"
              class="c-button"
            >
              Protect
            </button>
            <hr />`
        : ""}
      ${!isOnlyDevice
        ? html`<button
            @click="${() =>
              deleteDevice(userNumber, connection, device, isOnlyDevice, back)}"
            data-action="remove"
            class="c-button c-button--warning"
          >
            Delete ${isRecovery(device) ? "Recovery" : "Device"}
          </button>`
        : ""}
      ${!isOnlyDevice && isProtected(device)
        ? html`<p class="t-paragraph">
            This ${isRecovery(device) ? device.alias : "device"} is protected
            and you will be prompted to authenticate with it before removal.
          </p>`
        : ""}
      ${isOnlyDevice
        ? html`<p class="t-paragraph">
              This is your last device. You cannot remove it.
            </p>
            <p class="t-paragraph">
              Without devices your anchor would be inaccessible.
            </p>`
        : ""}
      <button @click="${back}" data-action="back" class="c-button">Back</button>
    </div>
  </article>
  ${footer}
`;

// We offer to protect unprotected recovery phrases only, although in the
// future we may offer to protect all devices
const shouldOfferToProtect = (device: DeviceData): boolean =>
  "recovery" in device.purpose &&
  "seed_phrase" in device.key_type &&
  !isProtected(device);

const isProtected = (device: DeviceData): boolean =>
  "protected" in device.protection;

const isRecovery = (device: DeviceData): boolean =>
  "recovery" in device.purpose;

// Get the list of devices from canister and actually display the page
export const deviceSettings = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData,
  isOnlyDevice: boolean
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;

  return new Promise((resolve) => {
    render(
      pageContent(userNumber, connection, device, isOnlyDevice, resolve),
      container
    );
  });
};

// Get a connection that's authenticated with the given device
// NOTE: this expects a recovery phrase device
const deviceConnection = async (
  connection: Connection,
  userNumber: bigint,
  device: DeviceData,
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
const deleteDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData,
  isOnlyDevice: boolean,
  back: () => void
) => {
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
          "recovery" in device.purpose ? "" : "device "
        }"${device.alias}"?`
      );
  if (!shouldProceed) {
    return;
  }

  // If the device is protected then we need to be authenticated with the device to remove it
  // NOTE: the user may be authenticated with the device already, but for consistency we still ask them to input their recovery phrase
  const removalConnection = isProtected(device)
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
      await back();
      return;
    }
    await removalConnection.remove(device.pubkey);
  });

  if (sameDevice) {
    // clear anchor and reload the page.
    // do not call "back", otherwise the management page will try to reload the list of devices which will cause an error
    localStorage.clear();
    location.reload();
    return;
  } else {
    back();
  }
};

/* Protect the device and re-render the device settings (with the updated device) */
const protectDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  device: DeviceData,
  isOnlyDevice: boolean,
  back: () => void
) => {
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
      await back();
      return;
    }

    await newConnection.update(device);
  });
  await deviceSettings(userNumber, connection, device, isOnlyDevice);
  back();
};
