import { render, html } from "lit-html";
import { IDPActor } from "../utils/idp_actor";
import {
  derBlobFromBlob,
  blobFromUint8Array,
  DerEncodedBlob,
} from "@dfinity/agent";
import { withLoader } from "../components/loader";
import { initLogout, logoutSection } from "../components/logout";
import { aboutLink } from "../components/aboutLink";
import { DeviceData, PublicKey } from "../../generated/internet_identity_types";
import { closeIcon } from "../components/icons";
import { displayError } from "../components/displayError";

const pageContent = (userNumber: bigint) => html`<style>
    #deviceLabel {
      margin-top: 1rem;
      margin-bottom: 0;
    }
  </style>
  <div class="container">
    <h1>Identity Management</h1>
    <p>
      You can view and manage your Internet identity and your registered devices
      here.
    </p>
    <label>User Number</label>
    <div class="highlightBox">${userNumber}</div>
    <label id="deviceLabel">Registered devices</label>
    <div id="deviceList"></div>
    ${logoutSection()}
  </div>
  ${aboutLink}`;

const deviceListItem = (alias: string) => html`
  <div class="deviceItemAlias">${alias}</div>
  <button type="button" class="deviceItemRemove">${closeIcon}</button>
`;

export const renderManage = (
  userNumber: bigint,
  connection: IDPActor
): void => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(userNumber), container);
  init(userNumber, connection);
};

const init = async (userNumber: bigint, connection: IDPActor) => {
  // TODO - Check alias for current identity, and populate #nameSpan
  initLogout();
  renderIdentities(userNumber, connection);
};

const renderIdentities = async (userNumber: bigint, connection: IDPActor) => {
  const deviceList = document.getElementById("deviceList") as HTMLElement;
  deviceList.innerHTML = ``;

  let identities: DeviceData[];
  try {
    identities = await IDPActor.lookup(userNumber);
  } catch (err) {
    await displayError({
      title: "Failed to list your devices",
      message:
        "An unexpected error occured when displaying your devices. Please try again",
      detail: err.toString(),
      primaryButton: "Try again",
    });
    return renderManage(userNumber, connection);
  }

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const identityElement = document.createElement("li");
    identityElement.className = "deviceItem";
    render(deviceListItem(identity.alias), identityElement);
    const isOnlyDevice = identities.length < 2;
    bindRemoveListener(
      userNumber,
      connection,
      identityElement,
      identity.pubkey,
      isOnlyDevice
    );
    list.appendChild(identityElement);
  });

  deviceList.appendChild(list);
};

const bindRemoveListener = (
  userNumber: bigint,
  connection: IDPActor,
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
      const shouldProceed = confirm(
        "This will remove your only remaining identity and may impact your ability to log in to accounts you have linked"
      );
      if (!shouldProceed) {
        return;
      }
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
