import { html, render } from "lit-html";
import { Connection } from "../../../utils/iiConnection";
import {
  addTentativeDevice,
  TentativeDeviceInfo,
} from "./registerTentativeDevice";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = (userNumber: bigint) => mainWindow({
  showLogo: false,
  showFooter: false,
  slot: html`
  <article>
    <hgroup>
      <h1 class="t-title t-title--main">Device Registration Not Enabled</h1>
      <p class="t-lead">
        Device registration is not enabled for the Identity Anchor
        <strong class="t-strong">${userNumber}</strong>. To enable device
        registration do the following:
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        On an <em>existing</em> device:<br />
        Log into <strong class="t-strong">https://identity.ic0.app</strong> with
        Identity Anchor
        <strong class="t-strong">${userNumber}</strong>
      </li>
      <li>
        On the <em>existing</em> device:<br />
        Click <strong class="t-strong">Add new device</strong>
      </li>
      <li>
        On the <em>existing</em> device:<br />
        Chose <strong class="t-strong">Remote Device</strong>
      </li>
      <li>
        On <em>this</em> device:<br />
        Press <strong class="t-strong">Retry</strong>
      </li>
    </ol>
    <div class="l-stack">
      <button id="deviceRegModeDisabledRetry" class="c-button">Retry</button>
      <button
        id="deviceRegModeDisabledCancel"
        class="c-button c-button--secondary"
      >
        Cancel
      </button>
    </div>
  </article>`
});

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
