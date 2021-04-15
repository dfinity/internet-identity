import { generateAddDeviceLink } from "../utils/generateAddDeviceLink";

let authenticationPollInterval;
export const initExistingUser = () => {
  bindListeners();
};

const toggleDialog = () => {
  const dialog = document.getElementById("loginDialog") as HTMLDialogElement;
  const isOpen = dialog.open;

  if (isOpen) dialog.close();
  else dialog.showModal();
};

const bindListeners = () => {
  // Buttons
  const dialogTrigger = document.getElementById(
    "dialogTrigger"
  ) as HTMLButtonElement;
  const closeDialog = document.getElementById(
    "closeDialog"
  ) as HTMLButtonElement;
  const toggleReconnect = document.getElementById(
    "toggleReconnect"
  ) as HTMLButtonElement;
  const toggleAddDevice = document.getElementById(
    "toggleAddDevice"
  ) as HTMLButtonElement;

  //   Hidden sections
  const reconnectSection = document.getElementById(
    "reconnectSection"
  ) as HTMLElement;
  const addDeviceLinkSection = document.getElementById(
    "addDeviceLinkSection"
  ) as HTMLElement;

  dialogTrigger.onclick = toggleDialog;
  closeDialog.onclick = toggleDialog;
  toggleReconnect.onclick = () => reconnectSection.classList.toggle("hidden");
  toggleAddDevice.onclick = () => {
    //   Generate link to add a user with an authenticated browser
    generateAddDeviceLink().then(link => {
      addDeviceLinkSection.classList.toggle("hidden");

      const addDeviceLink = document.getElementById(
        "addDeviceLink"
      ) as HTMLInputElement;
      addDeviceLink.value = link;
    });

    // Optional feature, not in current spec
    // authenticationPollInterval
    //   ? clearInterval(authenticationPollInterval)
    //   : pollForAuthentication();
  };
};

// Allows us to automatically redirect in the original browser
const pollForAuthentication = () => {
  authenticationPollInterval = setInterval(() => {
    // TODO - poll for authentication from adding in other browser
    Promise.reject()
      .then(() => {
        window.location.assign("/manage.html");
      })
      .catch(() => {});
  }, 5000);
};
