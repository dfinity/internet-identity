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
import { hasOwnProperty } from "../../utils/utils";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";

// The "device settings" page where users can view information about a device,
// remove a device, make a recovery phrase protected, etc.

// Actual page content. We display options for protecting recovery phrases or
// deleting general devices.
const pageContent = (
  userNumber: bigint,
  device: DeviceData,
  isOnlyDevice: boolean
) => html`
  <article id="deviceSettings" class="l-container c-card c-card--highlight">
    <h1 class="t-title">
      ${isRecovery(device) ? "" : "Device"} ${device.alias}
    </h1>

    <div class="l-section">
      ${shouldOfferToProtect(device)
        ? html` <p class="t-paragraph">
              By making your recovery phrase protected, you will need to input
              your recovery phrase to delete it.
            </p>
            <button data-action="protect" class="c-button">Protect</button>
            <hr />`
        : ""}
      ${!isOnlyDevice
        ? html`<button data-action="remove" class="c-button c-button--warning">
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
      <button data-action="back" class="c-button">Back</button>
    </div>
  </article>
  ${footer}
`;

// We offer to protect unprotected recovery phrases only, although in the
// future we may offer to protect all devices
const shouldOfferToProtect = (device: DeviceData): boolean =>
  hasOwnProperty(device.purpose, "recovery") &&
  hasOwnProperty(device.key_type, "seed_phrase") &&
  !isProtected(device);

const isProtected = (device: DeviceData): boolean =>
  "protected" in device.protection;

const isRecovery = (device: DeviceData): boolean =>
  hasOwnProperty(device.purpose, "recovery");

// Get the list of devices from canister and actually display the page
export const deviceSettings = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
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

// Initializes the device settings page
const init = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
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
            await resolve();
            return;
          }

          await newConnection.update(
            device.pubkey,
            device.alias,
            device.key_type,
            device.purpose,
            device.protection,
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
            await resolve();
            return;
          }
          await removalConnection.remove(device.pubkey);
        });

        if (sameDevice) {
          // clear anchor and reload the page.
          // do not resolve, otherwise the management page will try to reload the list of devices which will cause an error
          localStorage.clear();
          location.reload();
          return;
        }
        resolve();
      };
    }
  });
