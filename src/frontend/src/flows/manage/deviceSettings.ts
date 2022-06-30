import { render, html } from "lit-html";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { bufferEqual, IIConnection } from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { footer } from "../../components/footer";
import { DeviceData } from "../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../utils/utils";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";

// The "device settings" page where users can view information about a device,
// remove a device, make a recovery phrase protected, etc.

// The styling of the page
const style = () => html`<style></style> `;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const pageContent = (
  userNumber: bigint,
  device: DeviceData,
  isOnlyDevice: boolean
) => html`
  ${style()}
  <div class="container">
    <h1>Device Management</h1>
    <p><strong>${device.alias}<strong></p>

    ${
      shouldOfferToProtect(device)
        ? html`<button data-action="protect">Protect</button>
            <p>You will be asked for your phrase</p>`
        : ""
    }

    <button data-action="remove" ?disabled=${isOnlyDevice}>Delete Device</button>
    ${
      !isOnlyDevice && isProtected(device)
        ? html`<p>
            Your device is protected and you will be prompted to authenticate
            with it before removal
          </p>`
        : ""
    }
    ${isOnlyDevice ? "You cannot remove your last device" : ""}
    <button data-action="back">Back</button>
  </div>
  ${footer}
`;

// We offer to protect unprotected recovery phrases only, although in the
// future we may offer to protect all devices
const shouldOfferToProtect = (device: DeviceData): boolean =>
  hasOwnProperty(device.purpose, "recovery") && !isProtected(device);

const isProtected = (device: DeviceData): boolean =>
  "protected" in device.protection_type;

// Get the list of devices from canister and actually display the page
export const deviceSettings = async (
  userNumber: bigint,
  connection: IIConnection,
  device: DeviceData,
  isOnlyDevice: boolean
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(userNumber, device, isOnlyDevice), container);
  return init(userNumber, connection, device, isOnlyDevice);
};

// Get a connection that's authenticated with the given device
// NOTE: this expects a recovery phrase device
const deviceConnection = async (
  userNumber: bigint,
  device: DeviceData
): Promise<IIConnection | null> => {
  try {
    const loginResult = await phraseRecoveryPage(userNumber, device);
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

// Initializes the device settings page
const init = async (
  userNumber: bigint,
  connection: IIConnection,
  device: DeviceData,
  isOnlyDevice: boolean
): Promise<void> =>
  new Promise((resolve) => {
    const backButton = document.querySelector(
      "button[data-action=back]"
    ) as HTMLButtonElement;
    if (backButton !== null) {
      backButton.onclick = () => resolve();
    }

    const protectButton = document.querySelector(
      "button[data-action=protect]"
    ) as HTMLButtonElement;
    if (protectButton !== null) {
      protectButton.onclick = async () => {
        device.protection_type = { protected: null };

        // NOTE: we do _not_ need to be authenticated with the device in order to protect it,
        // but we do it to make sure one last time that the user can actually succesfully authenticate
        // with the device.
        const newConnection = await deviceConnection(userNumber, device);

        await withLoader(async () => {
          // if null then user canceled so we just redraw the manage page
          if (newConnection == null) {
            await resolve();
            return;
          }

          await newConnection.update(
            userNumber,
            device.pubkey,
            device.alias,
            device.key_type,
            device.purpose,
            device.protection_type,
            device.credential_id
          );
        });
        await deviceSettings(userNumber, connection, device, isOnlyDevice);
        resolve();
      };
    }

    const deleteButton = document.querySelector(
      "button[data-action=remove]"
    ) as HTMLButtonElement;
    if (deleteButton !== null) {
      deleteButton.onclick = async () => {
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
                hasOwnProperty(device.purpose, "recovery") ? "" : "device "
              }"${device.alias}"?`
            );
        if (!shouldProceed) {
          return;
        }

        // If the device is protected then we need to be authenticated with the device to remove it
        // NOTE: the user may be authenticated with the device already, but for consistency we still ask them to input their recovery phrase
        const removalConnection = isProtected(device)
          ? await deviceConnection(userNumber, device)
          : connection;

        await withLoader(async () => {
          // if null then user canceled so we just redraw the manage page
          if (removalConnection == null) {
            await resolve();
            return;
          }
          await removalConnection.remove(userNumber, device.pubkey);
        });
        resolve();
      };
    }
  });
