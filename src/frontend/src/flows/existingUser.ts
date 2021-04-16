import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";
import idp_actor from "../utils/idp_actor";
import { reconnectUser } from "../utils/reconnectUser";

export const initExistingUser = () => {
  bindListeners();
};

const bindListeners = () => {
  // Buttons
  const dialogTrigger = document.getElementById(
    "dialogTrigger"
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
};

const toggleDialog = () => {
  const dialog = document.getElementById("loginDialog") as HTMLDialogElement;
  if (idp_actor.userId && idp_actor.userId !== BigInt(1)) {
    const userIdInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;
    const userIdSection = document.getElementById(
      "userIdSection"
    ) as HTMLElement;
    userIdInput.value = idp_actor.userId.toString();
    userIdSection.classList.add("hidden");
  }
  const isOpen = dialog.open;

  if (isOpen) dialog.close();
  else dialog.showModal();
};

const handleLoginClick = async () => {
  // Attempt to delegate without prompting a signature
  reconnectUser(false);

  // Otherwise, open dialog for fallback options
  toggleDialog();
};

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
