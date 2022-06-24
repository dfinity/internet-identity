import { render, html } from "lit-html";
import { bufferEqual, IIConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { initLogout, logoutSection } from "../../components/logout";
import { navbar } from "../../components/navbar";
import { unreachable } from "../../utils/utils";
import { footer } from "../../components/footer";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import {
  closeIcon,
  settingsIcon,
  warningIcon,
  shieldIcon,
  shieldNotIcon,
} from "../../components/icons";
import { displayError } from "../../components/displayError";
import { setupRecovery } from "../recovery/setupRecovery";
import { hasOwnProperty, unknownToString } from "../../utils/utils";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";

// The styling of the page

const style = () => html`<style>
</style> `;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
// TODO: make isProtected more robust
// TODO: Delete device should be red and have dialog
const pageContent = (userNumber: bigint, device: DeviceData, isProtected: boolean) => html`
  ${style()}
  <div class="container">
    <h1>Device Management</h1>
    <p><strong>${device.alias}<strong></p>

    <button class="${isRecoveryDevice(device) ? "" : "hidden" }" data-action="toggle">${ isProtected ? "Unprotect" : "Make protected" }</button>
    <button data-action="remove">Delete Device</button>
    <button data-action="back">Back</button>
  </div>
  ${footer}
`;

// Whether or the user has registered a device as recovery
// TODO: make sure this doesn't trigger on non-phrases
const isRecoveryDevice = (device: DeviceData): boolean => hasOwnProperty(device.purpose, "recovery");

// Get the list of devices from canister and actually display the page
export const deviceSettings = async (
  userNumber: bigint,
  connection: IIConnection,
  device: DeviceData
): Promise<void> => {
    // TODO: re-fetch device here
  const container = document.getElementById("pageContent") as HTMLElement;

  const isProtected = hasOwnProperty(device.purpose, "recovery") && "protected" in device.protection_type;

  render(pageContent(userNumber, device, isProtected), container);
  return init(userNumber, connection, device, isProtected);
};

// Initializes the management page.
const init = async (
  userNumber: bigint,
  connection: IIConnection,
  device: DeviceData,
  isProtected: boolean
): Promise<void> => new Promise((resolve) => {
    const backButton = document.querySelector(
      "button[data-action=back]"
    ) as HTMLButtonElement;
    if (backButton !== null) {
      backButton.onclick = () =>
        resolve()
    }

    const toggleButton = document.querySelector(
      "button[data-action=toggle]"
    ) as HTMLButtonElement;
    if (toggleButton !== null) {
        // TODO: make sure connection is authenticated with device
        toggleButton.onclick = async () => {
            device.protection_type = "protected" in device.protection_type ? { unprotected: null } : { protected: null };

            // TODO: handle errors
            await withLoader(() => connection.update(
                userNumber,
                device.pubkey,
                device.alias,
                device.key_type,
                device.purpose,
                device.protection_type,
                device.credential_id
            ));
            await deviceSettings(userNumber, connection, device);
            resolve();
        }
    }

    const deleteButton = document.querySelector(
      "button[data-action=remove]"
    ) as HTMLButtonElement;
    if (deleteButton !== null) {
      deleteButton.onclick = async () => {
        await withLoader(() => connection.remove(userNumber, device.pubkey));
        resolve();
      }
    }
});
