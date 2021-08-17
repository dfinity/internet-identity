import { render, html } from "lit-html";
import { creationOptions, IIConnection } from "../utils/iiConnection";
import {
  derBlobFromBlob,
  blobFromUint8Array,
  DerEncodedBlob,
} from "@dfinity/candid";
import { withLoader } from "../components/loader";
import { initLogout, logoutSection } from "../components/logout";
import { aboutLink } from "../components/aboutLink";
import { faqLink } from "../components/faqLink";
import { DeviceData, PublicKey } from "../../generated/internet_identity_types";
import { closeIcon, warningIcon } from "../components/icons";
import { displayError } from "../components/displayError";
import { pickDeviceAlias } from "./addDevicePickAlias";
import { WebAuthnIdentity } from "@dfinity/identity";
import { setupRecovery } from "./recovery/setupRecovery";
import { hasOwnProperty } from "../utils/utils";

const pageContent = (userNumber: bigint, devices: DeviceData[]) => html`<style>
    .nagBox {
      display: flex;
      align-items: center;
      gap: 1rem;
      padding: 1rem;
      margin-bottom: 2rem;
      box-sizing: border-box;
      border-style: double;
      border-width: 2px;
      border-radius: 4px;
      border-image-slice: 1;
      outline: none;
      border-image-source: linear-gradient(
        270.05deg,
        #29abe2 10.78%,
        #522785 22.2%,
        #ed1e79 42.46%,
        #f15a24 59.41%,
        #fbb03b 77.09%
      );
    }
    .nagIcon {
      align-self: flex-start;
    }
    .recoveryNag {
      display: flex;
      flex-direction: column;
    }
    .recoveryNagTitle {
      font-weight: 600;
      font-size: 1.1rem;
    }
    .recoveryNagMessage {
      margin-top: 0.5rem;
      margin-bottom: 1rem;
      font-size: 1rem;
    }
    .recoveryNagButton {
      padding: 0.2rem 0.4rem;
      border-radius: 2px;
      width: fit-content;
      align-self: flex-end;
      margin: 0;
    }
    .labelWithAction {
      margin-top: 1rem;
      display: flex;
      justify-content: space-between;
    }

    .labelWithAction label {
      margin: 0;
    }

    .labelAction {
      padding: 0;
      border: none;
      display: inline;
      width: auto;
      margin: 0;
      cursor: pointer;
      color: #387ff7;
      font-size: 12px;
      font-family: "Montserrat", sans-serif;
      text-align: right;
      font-weight: 600;
    }
    .labelAction::before {
      content: "+";
      margin-right: 3px;
      color: #387ff7;
    }
  </style>
  <div class="container">
    <h1>Anchor Management</h1>
    <p>
      You can view and manage this Identity Anchor and its added devices here.
    </p>
    ${shouldNag(devices) ? recoveryNag() : undefined}
    <label>Identity Anchor</label>
    <div class="highlightBox">${userNumber}</div>
    <div class="labelWithAction">
      <label id="deviceLabel">Added devices</label>
      <button class="labelAction" id="addAdditionalDevice">
        ADD NEW DEVICE
      </button>
    </div>
    <div id="deviceList"></div>
    ${shouldNag(devices)
      ? undefined
      : html`
          <div class="labelWithAction">
            <label id="deviceLabel">Recovery mechanisms</label>
            <button class="labelAction" id="addRecovery">
              ADD RECOVERY MECHANISM
            </button>
          </div>
          <div id="recoveryList"></div>
        `}
    ${logoutSection()}
  </div>
  <div id="navbar" >
  ${aboutLink} &middot; ${faqLink}
  </div>`;

const deviceListItem = (alias: string) => html`
  <div class="deviceItemAlias">${alias}</div>
  <button type="button" class="deviceItemRemove">${closeIcon}</button>
`;

const recoveryNag = () => html`
  <div class="nagBox">
    <div class="nagIcon">${warningIcon}</div>
    <div class="recoveryNag">
      <div class="recoveryNagTitle">Recovery Mechanism</div>
      <div class="recoveryNagMessage">
        Add a recovery mechanism to help protect this Identity Anchor.
      </div>
      <button id="addRecovery" class="primary recoveryNagButton">
        Add Recovery Key
      </button>
    </div>
  </div>
`;

export const renderManage = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;

  let devices: DeviceData[];
  try {
    devices = await withLoader(() => IIConnection.lookupAll(userNumber));
  } catch (err) {
    await displayError({
      title: "Failed to list your devices",
      message:
        "An unexpected error occurred when displaying your devices. Please try again",
      detail: err.toString(),
      primaryButton: "Try again",
    });
    return renderManage(userNumber, connection);
  }
  render(pageContent(userNumber, devices), container);
  init(userNumber, connection, devices);
};

const init = async (
  userNumber: bigint,
  connection: IIConnection,
  devices: DeviceData[]
) => {
  // TODO - Check alias for current identity, and populate #nameSpan
  initLogout();
  const setupRecoveryButton = document.querySelector(
    "#addRecovery"
  ) as HTMLButtonElement;

  setupRecoveryButton.onclick = async () => {
    await setupRecovery(userNumber, connection);
    renderManage(userNumber, connection);
  };

  const addAdditionalDevice = document.querySelector(
    "#addAdditionalDevice"
  ) as HTMLButtonElement;

  addAdditionalDevice.onclick = async () => {
    let newDevice: WebAuthnIdentity;
    try {
      newDevice = await WebAuthnIdentity.create({
        publicKey: creationOptions(devices),
      });
    } catch (error) {
      await displayError({
        title: "Failed to add new device",
        message: html`
          We failed to add your new device.<br />
          If you're trying to add a device that is not attached to this machine
          try following the instructions at<br />
          <a
            target="_blank"
            href="https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html#_add_a_device"
            >https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html#_add_a_device</a
          >
        `,
        detail: error.message,
        primaryButton: "Back to manage",
      });
      return renderManage(userNumber, connection);
    }
    const deviceName = await pickDeviceAlias();
    if (deviceName === null) {
      return renderManage(userNumber, connection);
    }
    // TODO check whether newDevice is already registered,
    // or better, pass existing devices to `.create` so that they
    // cannot be added again
    try {
      await withLoader(() =>
        connection.add(
          userNumber,
          deviceName,
          { unknown: null },
          { authentication: null },
          newDevice.getPublicKey().toDer(),
          newDevice.rawId
        )
      );
    } catch (error) {
      await displayError({
        title: "Failed to add the new device",
        message:
          "We failed to add the new device to this Identity Anchor. Please try again",
        detail: error.message,
        primaryButton: "Back to manage",
      });
    }
    renderManage(userNumber, connection);
  };

  renderDevices(userNumber, connection, devices);
};

const renderDevices = async (
  userNumber: bigint,
  connection: IIConnection,
  devices: DeviceData[]
) => {
  const list = document.createElement("ul");
  const recoveryList = document.createElement("ul");

  devices.forEach((device) => {
    const identityElement = document.createElement("li");
    identityElement.className = "deviceItem";
    render(deviceListItem(device.alias), identityElement);
    const isOnlyDevice = devices.length < 2;
    bindRemoveListener(
      userNumber,
      connection,
      identityElement,
      device.pubkey,
      isOnlyDevice
    );
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

const bindRemoveListener = (
  userNumber: bigint,
  connection: IIConnection,
  listItem: HTMLElement,
  publicKey: PublicKey,
  isOnlyDevice: boolean
) => {
  const button = listItem.querySelector("button") as HTMLButtonElement;
  button.onclick = async () => {
    const pubKey: DerEncodedBlob = derBlobFromBlob(
      blobFromUint8Array(new Uint8Array(publicKey))
    );
    const sameDevice = connection.identity
      .getPublicKey()
      .toDer()
      .equals(pubKey);

    if (sameDevice) {
      const shouldProceed = confirm(
        "This will remove your current device and you will be logged out"
      );
      if (!shouldProceed) {
        return;
      }
    }

    if (isOnlyDevice) {
      return alert("You can not remove your last device.");
    }

    // Otherwise, remove identity
    try {
      await withLoader(() => connection.remove(userNumber, publicKey));
      if (sameDevice) {
        localStorage.clear();
        location.reload();
      }
      renderManage(userNumber, connection);
    } catch (err) {
      await displayError({
        title: "Failed to remove the device",
        message:
          "An unexpected error occured when trying to remove the device. Please try again",
        detail: err.toString(),
        primaryButton: "Back to Manage",
      });
      renderManage(userNumber, connection);
    }
  };
};

const shouldNag = (devices: DeviceData[]): boolean =>
  !devices.some((device) => hasOwnProperty(device.purpose, "recovery"));
