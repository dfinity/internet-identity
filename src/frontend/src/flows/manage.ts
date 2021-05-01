import { render, html } from "lit-html";
import { IDPActor } from "../utils/idp_actor";
import { derBlobFromBlob, blobFromUint8Array } from "@dfinity/agent";
import { withLoader } from "../components/loader";
import { initLogout, logoutSection } from "../components/logout";
import { aboutLink } from "../components/aboutLink";
import { DeviceData } from "../../generated/idp_types";
import { closeIcon } from "../components/icons";

const pageContent = () => html`<style>
  #deviceLabel {
    margin-top: 1rem;
    margin-bottom: 0;
    font-size: 1rem;
    font-weight: 500;
  }
  </style>
  <div class="container">
    <h1>Identity Management</h1>
    <p>You can view and manage your Internet identity and your registered devices here.</p>
    <h3>Your User Number is <span id="userNumberSpan"></span></h3>
    <label id="deviceLabel">Registered devices:</label>
    <div id="deviceList"></div>
    ${logoutSection()}
  </div>
  ${aboutLink}
  <web-dialog id="prompt">
    <form action="" id="prompt-form">
      <p id="prompt-text"></p>
      <p class="details"></p>
      <input type="text" id="prompt-input" />
      <div class="flex row">
        <button type="submit">Confirm</button>
        <button type="button" id="prompt-cancel">Cancel</button>
      </div>
    </form>
  </web-dialog>
  <web-dialog id="confirm">
    <form action="" id="confirm-form">
      <p id="confirm-text"></p>
      <p class="details"></p>
      <div class="flex row">
        <button type="submit">Confirm</button>
        <button type="button" id="confirm-cancel">Cancel</button>
      </div>
    </form>
  </web-dialog>`;

const deviceListItem = (alias) => html`
<div class="deviceItem">
  <div class="deviceItemAlias">${alias}</div>
  <button type="button" class="deviceItemRemove">
    ${closeIcon}
  </button>
</div>
`;

export const renderManage = (userNumber: bigint, connection: IDPActor) => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(), container);
  init(userNumber, connection);
};

export const init = async (userNumber, connection) => {
  // TODO - Check alias for current identity, and populate #nameSpan
  displayUserNumber(userNumber);
  initLogout();
  renderIdentities(connection, userNumber);
};

const displayUserNumber = (userNumber: BigInt) => {
  const userNumberElem = document.getElementById("userNumberSpan") as HTMLElement;
  userNumberElem.innerHTML = userNumber.toString();
};

const renderIdentities = async (connection, userNumber) => {
  const deviceList = document.getElementById("deviceList") as HTMLElement;
  deviceList.innerHTML = ``;

  const identities = await IDPActor.lookup(userNumber);

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const identityElement = document.createElement("li");
    identityElement.className = "flex row justify-between";
    render(deviceListItem(identity.alias), identityElement);
    bindRemoveListener(userNumber, connection, identityElement, identity.pubkey);
    list.appendChild(identityElement);
  });

  deviceList.appendChild(list);
};

const bindRemoveListener = (
  userNumber: bigint,
  connection: IDPActor,
  listItem: HTMLElement,
  publicKey
) => {
  const button = listItem.querySelector("button") as HTMLButtonElement;
  button.onclick = async () => {
    // Make sure we're not removing our last identity
    const identities = document.querySelectorAll("#identityList li");

    const sameDevice = connection.identity.getPublicKey().toDer().equals(derBlobFromBlob(blobFromUint8Array(publicKey)));

    if (sameDevice) {
      const shouldProceed = confirm(
        "This will remove your current device and you will be logged out"
      );
      if (!shouldProceed) {
        return;
      }
    }

    if (identities.length <= 1) {
      const shouldProceed = confirm(
        "This will remove your only remaining identity and may impact your ability to log in to accounts you have linked"
      );
      if (!shouldProceed) {
        return;
      }
    }

    // Otherwise, remove identity
    await withLoader(() =>
      connection.remove(userNumber, publicKey).then(() => {
        listItem.parentElement?.removeChild(listItem);
      })
    );

    if (sameDevice) {
      localStorage.clear();
      location.reload();
    }
  };
};
