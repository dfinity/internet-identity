import { render, html } from "lit-html";
import { identityListItem } from "../components/identityListItem";
import { IDPActor } from "../utils/idp_actor";
import { derBlobFromBlob, blobFromUint8Array } from "@dfinity/agent";
import { withLoader } from "../components/loader";
import { initLogout, logoutSection } from "../components/logout";

const pageContent = () => html`<style>
    #userIdSection {
      padding-top: 0;
      padding-bottom: 0;
    }
    button[title="Remove identity"] {
      position: relative;
      padding: 0.25rem;
      height: 1rem;
      width: 1rem;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
    }
    @media (prefers-color-scheme: dark) {
      path {
        stroke: var(--text-color);
      }
    }
    #identityList {
      max-width: 500px;
    }
    #identityList li {
      border-bottom: 1px solid var(--text-color);
    }
  </style>
  <div class="container">
    <section id="intro">
      <h1>Identity Management</h1>
      <p>You can view and manage your Internet Computer identities here.</p>
    </section>
    <section id="userIdSection" class="hidden">
      <h3>Your User Number is <span id="userIdSpan"></span></h3>
    </section>
    <section id="identityList"></section>
    ${logoutSection()}
  </div>
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

export const renderManage = (userId: bigint, connection: IDPActor) => {
  const container = document.getElementById("pageContent") as HTMLElement;

  render(pageContent(), container);
  init(userId, connection);
};

export const init = async (userId, connection) => {
  // TODO - Check alias for current identity, and populate #nameSpan
  displayUserId(userId);
  initLogout();
  renderIdentities(connection, userId);
};

const displayUserId = (userId: BigInt) => {
  const userIdElem = document.getElementById("userIdSpan") as HTMLElement;
  userIdElem.innerHTML = userId.toString();
  const userIdSection = document.getElementById("userIdSection") as HTMLElement;
  userIdSection.classList.remove("hidden");
};

const renderIdentities = async (connection, userId) => {
  const identityList = document.getElementById("identityList") as HTMLElement;
  identityList.innerHTML = ``;

  const identities = await IDPActor.lookup(userId);

  const list = document.createElement("ul");

  identities.forEach((identity) => {
    const identityElement = document.createElement("li");
    identityElement.className = "flex row justify-between";
    identityElement.innerHTML = identityListItem(identity.alias);
    bindRemoveListener(userId, connection, identityElement, identity.pubkey);
    list.appendChild(identityElement);
  });

  identityList.appendChild(list);
};

const bindRemoveListener = (
  userId: bigint,
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
      connection.remove(userId, publicKey).then(() => {
        listItem.parentElement?.removeChild(listItem);
      })
    );

    if (sameDevice) {
      localStorage.clear();
      location.reload();
    }
  };
};
