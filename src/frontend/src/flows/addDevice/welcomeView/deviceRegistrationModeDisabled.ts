import { html, render } from "lit-html";
import { Connection } from "../../../utils/iiConnection";
import {
  addTentativeDevice,
  TentativeDeviceInfo,
} from "./registerTentativeDevice";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = (userNumber: bigint) => {
  const pageContentSlot = html` <article>
    <hgroup>
      <h1 class="t-title t-title--main">
        Enroll this device from a registered device
      </h1>
      <p class="t-lead">
        If you want to add this device, follow the instructions below
        <strong class="t-strong"
          >on a device that Internet Identity already trusts:</strong
        >
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        Log into <strong class="t-strong">identity.ic0.app</strong> with your
        Identity Anchor (<strong class="t-strong">${userNumber}</strong>)
      </li>
      <li>
        Once you are logged in, click “<strong class="t-string"
          >Add new device</strong
        >”
      </li>
      <li>Select “<strong class="t-string">Browser</strong>”</li>
    </ol>
    <p class="t-paragraph t-strong">Then, press Retry below.</p>
    <div class="l-stack">
      <button id="deviceRegModeDisabledRetry" class="c-button">Retry</button>
      <button
        id="deviceRegModeDisabledCancel"
        class="c-button c-button--secondary"
      >
        Cancel
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};
/**
 * Error page which is shown if the identy anchor does not have device registration mode enabled.
 * It shows instructions to the user on how to continue.
 * @param tentativeDeviceInfo Information about the device to be added so that the user does not have to enter everything again after enabling device registration mode.
 */
export const deviceRegistrationDisabledInfo = async (
  connection: Connection,
  tentativeDeviceInfo: TentativeDeviceInfo
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(tentativeDeviceInfo[0]), container);
  return init(connection, tentativeDeviceInfo);
};

const init = async (
  connection: Connection,
  tentativeDeviceInfo: TentativeDeviceInfo
): Promise<void> => {
  const cancelButton = document.getElementById(
    "deviceRegModeDisabledCancel"
  ) as HTMLButtonElement;

  cancelButton.onclick = () => {
    // TODO L2-309: do this without reload
    window.location.reload();
  };

  const retryButton = document.getElementById(
    "deviceRegModeDisabledRetry"
  ) as HTMLButtonElement;

  retryButton.onclick = async () => {
    await addTentativeDevice(connection, tentativeDeviceInfo);
  };
};
