import { render, html } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { unreachable } from "../../utils/utils";
import { footer } from "../../components/footer";
import { DeviceData } from "../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../utils/utils";
import { phraseRecoveryPage } from "../recovery/recoverWith/phrase";

// The styling of the page

const style = () => html`<style></style> `;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
// TODO: Delete device should be red and have dialog for confirmation
// TODO: make sure user cannot remove last device
// TODO: warning if user will get logged out
const pageContent = (userNumber: bigint, device: DeviceData) => html`
  ${style()}
  <div class="container">
    <h1>Device Management</h1>
    <p><strong>${device.alias}<strong></p>

    ${
      shouldOfferToProtect(device)
        ? html`<button data-action="protect">Protect</button>`
        : ""
    }

    <button data-action="remove">Delete Device</button>
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
  device: DeviceData
): Promise<void> => {
  // TODO: re-fetch device here?
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(userNumber, device), container);
  return init(userNumber, connection, device);
};

const getRemovalConnection = async (
  originalConnection: IIConnection,
  userNumber: bigint,
  device: DeviceData
): Promise<IIConnection | null> => {
  // TODO: don't go through this if user is authenticated with device already
  if (isProtected(device)) {
    // TODO: what happens on error?
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
  } else {
    return originalConnection;
  }
};

// Initializes the management page.
const init = async (
  userNumber: bigint,
  connection: IIConnection,
  device: DeviceData
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
      // TODO: make sure that user inputs recovery before protecting
      protectButton.onclick = async () => {
        device.protection_type =
          "protected" in device.protection_type
            ? { unprotected: null }
            : { protected: null };

        // TODO: handle errors
        await withLoader(() =>
          connection.update(
            userNumber,
            device.pubkey,
            device.alias,
            device.key_type,
            device.purpose,
            device.protection_type,
            device.credential_id
          )
        );
        await deviceSettings(userNumber, connection, device);
        resolve();
      };
    }

    const deleteButton = document.querySelector(
      "button[data-action=remove]"
    ) as HTMLButtonElement;
    if (deleteButton !== null) {
      deleteButton.onclick = async () => {
        const removalConnection = await getRemovalConnection(
          connection,
          userNumber,
          device
        );

        // if null then user canceled so we just redraw the manage page

        await withLoader(() => {
          if (removalConnection === null) {
            resolve();
            // TODO this is fishy
            return Promise.resolve();
          }
          return removalConnection.remove(userNumber, device.pubkey);
        });
        resolve();
      };
    }
  });
