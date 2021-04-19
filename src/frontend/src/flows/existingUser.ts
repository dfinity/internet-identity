import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";
import idp_actor from "../utils/idp_actor";
import oauth from "../utils/oath";

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
  const userId = idp_actor.userId;
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
  if (idp_actor.userId) {
    // Make the user reauthenticate
    await idp_actor.reconnect().then(() => postReconnect());
  }

  // Otherwise, open dialog for fallback options
  toggleDialog();
};

const handleReconnectClick = async () => {
  const userIdInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;

  const userId = BigInt(userIdInput.value);
  if (userId) {
    idp_actor.userId = userId;
    // Make the user reauthenticate
    await idp_actor.reconnect().then(() => postReconnect());
  } else {
    console.error("Failed to login with that user #");
  }
};

function postReconnect() {
  if (window.location.href.match(/authorize/)) {
    oauth();
  } else {
    window.location.assign("/manage");
  }
}

const handleToggleDeviceClick = () => {
  // Inputs
  const userIdInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;
  const addDeviceLinkSection = document.getElementById(
    "addDeviceLinkSection"
  ) as HTMLElement;

  const userId = BigInt(userIdInput.value);

  // Generate link to add a user with an authenticated browser
  generateAddDeviceLink(userId).then((link) => {
    addDeviceLinkSection.classList.toggle("hidden");

    const addDeviceLink = document.getElementById(
      "addDeviceLink"
    ) as HTMLInputElement;
    addDeviceLink.value = link;
  });
};
