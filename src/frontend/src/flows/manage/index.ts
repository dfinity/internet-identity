import { render, html } from "lit-html";
import { AuthenticatedConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { initLogout, logoutSection } from "../../components/logout";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { deviceSettings } from "./deviceSettings";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../generated/internet_identity_types";
import { settingsIcon } from "../../components/icons";
import { displayError } from "../../components/displayError";
import { setupRecovery } from "../recovery/setupRecovery";
import { hasOwnProperty } from "../../utils/utils";
import { pollForTentativeDevice } from "../addDevice/manage/pollForTentativeDevice";
import { chooseDeviceAddFlow } from "../addDevice/manage";
import { addLocalDevice } from "../addDevice/manage/addLocalDevice";
import { warnBox } from "../../components/warnBox";

const displayFailedToListDevices = (error: Error) =>
  displayError({
    title: "Failed to list your devices",
    message:
      "An unexpected error occurred when displaying your devices. Please try again",
    detail: error.toString(),
    primaryButton: "Try again",
  });

// The maximum number of authenticator (non-recovery) devices we allow.
// The canister limits the _total_ number of devices (recovery included) to 10,
// and we (the frontend) only allow user one recovery device per type (phrase, fob),
// which leaves room for 8 authenticator devices.
const MAX_AUTHENTICATORS = 8;
const numAuthenticators = (devices: DeviceData[]) =>
  devices.filter((device) => hasOwnProperty(device.purpose, "authentication"))
    .length;

// The styling of the page

const style = () => html`<style>
  .labelWithAction {
    margin-top: 1rem;
    margin-bottom: 0.5rem;
    display: flex;
    justify-content: space-between;
  }

  .labelWithAction button {
    text-align: right;
  }

  .labelWithAction .labelWithAction {
    margin: 0;
    display: flex;
    flex-direction: row;
    align-items: center;
  }

  .labelWithAction .labelWithAction span {
    /* global span selector is set to 1.2rem (...), which is too big */
    font-size: 1rem;
  }

  .labelWithAction .labelWithAction span.addedDevicesCounter {
    /* we can't just select the class because the previous selector is more specific */
    font-size: 0.7rem;
  }

  .labelAction {
    padding: 0;
    border: none;
    display: inline;
    width: auto;
    margin: 0;
    cursor: pointer;
  }

  .labelActionText {
    font-size: 12px;
    font-family: "Montserrat", sans-serif;
    text-align: right;
    font-weight: 600;
    color: #1f6ef4;
  }

  .addedDevices {
    font-weight: 500;
    font-size: 1rem;
    margin: 0 0 0.2rem;
  }

  .labelActionText::before {
    content: "+";
    margin-right: 3px;
  }

  .labelAction:disabled .labelActionText {
    color: var(--grey-100);
  }

  .tooltip {
    visibility: hidden;
    opacity: 0;
    display: inline-block;
    max-width: 200px;
    font-size: 12px;
    position: absolute;
    z-index: 1;
    background: var(--grey-100);
    padding: 10px;
    border-radius: 10px;
    text-align: center;
  }

  .labelAction:disabled:hover .tooltip {
    visibility: visible;
    opacity: 1;
    transition: opacity 0.2s ease-in;
  }
  .addedDevicesCounter {
    margin-left: 0.1em;
  }

  .addedDevicesCounter:hover .tooltip {
    visibility: visible;
    opacity: 1;
    transition: opacity 0.2s ease-in;
  }

  .label {
    font-size: 1.2rem;
    font-weight: 500;
    margin: 0;
  }

  .highlightBox {
    margin-top: 1rem;
  }

  .labelAddedDevices {
    flex-grow: 1;
    display: flex;
  }
</style> `;

// Actual page content. We display the Identity Anchor and the list of
// (non-recovery) devices. Additionally, if the user does _not_ have any
// recovery devices, we display a warning "nag box" and suggest to the user
// that they add a recovery device. If the user _does_ have at least one
// recovery device, then we do not display a "nag box", but we list the
// recovery devices.
const pageContent = (userNumber: bigint, devices: DeviceData[]) => html`
  ${style()}
  <section class="container">
    <h1>Anchor Management</h1>
    <p>
      You can view and manage this Identity Anchor and its added devices here.
    </p>
    ${!hasRecoveryDevice(devices) ? recoveryNag() : undefined}
    <aside>
      <h2 class="label">Identity Anchor</h2>
      <div class="highlightBox">${userNumber}</div>
    </aside>

    <aside>
      <div class="labelWithAction">
        <div class="labelAddedDevices">
          <h2 class="label addedDevices">Added devices</h2>
          <span class="addedDevicesCounter"
            ><span class="tooltip"
              >You can register up to ${MAX_AUTHENTICATORS} authenticator
              devices (recovery devices excluded)</span
            >(${numAuthenticators(devices)}/${MAX_AUTHENTICATORS})</span
          >
        </div>
        <button
          ?disabled=${numAuthenticators(devices) >= MAX_AUTHENTICATORS}
          class="labelAction"
          id="addAdditionalDevice"
        >
          <span class="tooltip"
            >You can register up to ${MAX_AUTHENTICATORS} authenticator devices.
            Remove a device before you can add a new one.</span
          >
          <span class="labelActionText">ADD NEW DEVICE</span>
        </button>
      </div>
      <div id="deviceList"></div>
    </aside>
    <aside>
      ${!hasRecoveryDevice(devices)
        ? undefined
        : html`
            <div class="labelWithAction">
              <h2 class="label">Recovery mechanisms</h2>
              <button class="labelAction" id="addRecovery">
                <span class="labelActionText">ADD RECOVERY MECHANISM</span>
              </button>
            </div>
            <div id="recoveryList"></div>
          `}
    </aside>
    ${logoutSection()} ${navbar}
  </section>
  ${footer}
`;

const deviceListItem = (device: DeviceData) => html`
  <div class="deviceItemAlias">${device.alias}</div>
  <button
    type="button"
    aria-label="settings"
    data-action="settings"
    class="deviceItemAction"
  >
    ${settingsIcon}
  </button>
`;

const recoveryNag = () =>
  html`${warnBox({
    title: "Recovery Mechanism",
    message: html`Add a recovery mechanism to help protect this Identity Anchor.`,
    componentType: "div",
    slot: html`<button id="addRecovery" class="primary warnButton">
      Add Recovery
    </button>`,
  })}`;

// Get the list of devices from canister and actually display the page
export const renderManage = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;

  let anchorInfo: IdentityAnchorInfo;
  try {
    anchorInfo = await withLoader(() => connection.getAnchorInfo());
  } catch (error: unknown) {
    await displayFailedToListDevices(
      error instanceof Error ? error : unknownError()
    );
    return renderManage(userNumber, connection);
  }
  if (anchorInfo.device_registration.length !== 0) {
    // we are actually in a device registration process
    await pollForTentativeDevice(userNumber, connection);
  } else {
    render(pageContent(userNumber, anchorInfo.devices), container);
    init(userNumber, connection, anchorInfo.devices);
  }
};

// Initializes the management page.
const init = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
) => {
  // TODO - Check alias for current identity, and populate #nameSpan
  initLogout();

  // Add the buttons for adding devices and recovery mechanism

  // Add device
  const addAdditionalDeviceButton = document.querySelector(
    "#addAdditionalDevice"
  ) as HTMLButtonElement;
  addAdditionalDeviceButton.onclick = async () => {
    const nextAction = await chooseDeviceAddFlow();
    if (nextAction === null) {
      // user clicked 'cancel'
      await renderManage(userNumber, connection);
      return;
    }
    switch (nextAction) {
      case "local": {
        await addLocalDevice(userNumber, connection, devices);
        return;
      }
      case "remote": {
        await pollForTentativeDevice(userNumber, connection);
        return;
      }
    }
  };

  // Add recovery
  const setupRecoveryButton = document.querySelector(
    "#addRecovery"
  ) as HTMLButtonElement;
  setupRecoveryButton.onclick = async () => {
    await setupRecovery(userNumber, connection);
    renderManage(userNumber, connection);
  };
  renderDevices(userNumber, connection, devices);
};

const renderDevices = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  devices: DeviceData[]
) => {
  const list = document.createElement("ul");
  const recoveryList = document.createElement("ul");
  const isOnlyDevice = devices.length < 2;

  devices.forEach((device) => {
    const identityElement = document.createElement("li");
    identityElement.className = "deviceItem";

    render(deviceListItem(device), identityElement);
    const buttonSettings = identityElement.querySelector(
      "button[data-action=settings]"
    ) as HTMLButtonElement;
    if (buttonSettings !== null) {
      buttonSettings.onclick = async () => {
        await deviceSettings(
          userNumber,
          connection,
          device,
          isOnlyDevice
        ).catch((e) =>
          displayError({
            title: "Could not edit device",
            message: "An error happened on the settings page.",
            detail: e.toString(),
            primaryButton: "Ok",
          })
        );
        await renderManage(userNumber, connection);
      };
    }
    hasOwnProperty(device.purpose, "recovery")
      ? recoveryList.appendChild(identityElement)
      : list.appendChild(identityElement);
  });
  const deviceList = document.getElementById("deviceList") as HTMLElement;
  deviceList.innerHTML = ``;
  deviceList.appendChild(list);

  const recoveryDevices = document.getElementById(
    "recoveryList"
  ) as HTMLElement;

  if (recoveryDevices !== null) {
    recoveryDevices.innerHTML = ``;
    recoveryDevices.appendChild(recoveryList);
  }
};

// Whether or the user has registered a device as recovery
const hasRecoveryDevice = (devices: DeviceData[]): boolean =>
  devices.some((device) => hasOwnProperty(device.purpose, "recovery"));

const unknownError = (): Error => {
  return new Error("Unknown error");
};
