import { blobFromBuffer, blobFromUint8Array, derBlobFromBlob } from "@dfinity/agent";
import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";
import idp_actor, { IDPActor } from "../utils/idp_actor";
import oauth from "../utils/oath";
import { navigateTo } from "../utils/router";
import { getUserId, setUserId } from "../utils/userId";

export const initExistingUser = () => {
  bindListeners();
};

const bindListeners = () => {
  // Buttons
  const dialogTrigger = document.getElementById(
    "dialogTrigger"
  ) as HTMLButtonElement;
  const reconnectTrigger = document.getElementById(
    "reconnectTrigger"
  ) as HTMLButtonElement;
  const closeDialog = document.getElementById(
    "closeDialog"
  ) as HTMLButtonElement;
  const toggleAddDevice = document.getElementById(
    "toggleAddDevice"
  ) as HTMLButtonElement;

  dialogTrigger.onclick = handleLoginClick;
  closeDialog.onclick = toggleDialog;
  toggleAddDevice.onclick = handleToggleDeviceClick;
  reconnectTrigger.onclick = handleReconnectClick;
};

const toggleDialog = () => {
  const dialog = document.getElementById("loginDialog") as HTMLDialogElement;
  const userId = getUserId();
  if (userId) {
    const userIdInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;
    const userIdSection = document.getElementById(
      "userIdSection"
    ) as HTMLElement;
    userIdInput.value = userId.toString();
    userIdSection.classList.add("hidden");
  }
  const isOpen = dialog.hasAttribute("open");

  if (isOpen) dialog.removeAttribute("open");
  else dialog.open = true;
};

const handleLoginClick = async () => {
  const userId = getUserId();
  if (userId) {
    // TODO: Greet returning user, or offer to login as different user
    // Make the user reauthenticate
    await IDPActor.reconnect(userId).then(postReconnect(userId));
  } else {
    // Otherwise, open dialog for fallback options
    toggleDialog();
  }
};

const handleReconnectClick = async () => {
  const userIdInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;

  const userId = BigInt(userIdInput.value);
  if (userId) {
    IDPActor.reconnect(userId).then(postReconnect(userId));
  } else {
    console.error("Failed to login with that user #");
  }
};

const postReconnect = (userId: bigint) => (connection: IDPActor) => {
  idp_actor.connection = connection;
  setUserId(userId);
  if (window.location.href.match(/authorize/)) {
    oauth(userId, connection);
  } else {
    navigateTo("/manage");
  }
}

let loginInterval: number;

const handleToggleDeviceClick = () => {
  clearInterval(loginInterval);
  // Inputs
  const userIdInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;
  const addDeviceLinkSection = document.getElementById(
    "addDeviceLinkSection"
  ) as HTMLElement;

  const userId = BigInt(userIdInput.value);

  // Generate link to add a user with an authenticated browser
  generateAddDeviceLink(userId).then(({link, publicKey}) => {
    setUserId(userId);
    addDeviceLinkSection.classList.toggle("hidden");

    const addDeviceLink = document.getElementById(
      "addDeviceLink"
    ) as HTMLInputElement;
    addDeviceLink.value = link;

    loginInterval = window.setInterval(async () => {
      console.log("checking if authenticated");
      try {
        let devices = await IDPActor.lookup(userId);
        let matchedDevice = devices.find(deviceData =>
          derBlobFromBlob(blobFromUint8Array(Buffer.from(deviceData.pubkey))).equals(publicKey)
        );
        if (matchedDevice !== undefined) {
          window.clearInterval(loginInterval)
          IDPActor.reconnect(userId).then(postReconnect(userId))
        }
      } catch (error) {
        console.error(error);
      }
    }, 2500);
  });
};
