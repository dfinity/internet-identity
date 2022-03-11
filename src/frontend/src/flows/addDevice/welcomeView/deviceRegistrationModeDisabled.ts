import { html, render } from "lit-html";
import {
  addTentativeDevice,
  TentativeDeviceInfo,
} from "./registerTentativeDevice";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Device Registration Not Enabled</h1>
    <p>
      Device registration is not enabled for the Identity Anchor
      <strong>${userNumber}</strong>. To enable device registration do the
      following:
    </p>
    <ol class="instruction-steps">
      <li>
        On an <em>existing</em> device:<br />
        Log into <strong>https://identity.ic0.app</strong> with Identity Anchor
        <strong>${userNumber}</strong>
      </li>
      <li>
        On the <em>existing</em> device:<br />
        Click <strong>Add new device</strong>
      </li>
      <li>
        On the <em>existing</em> device:<br />
        Chose <strong>Remote Device</strong>
      </li>
      <li>
        On <em>this</em> device:<br />
        Press <strong>Retry</strong>
      </li>
    </ol>
    <button id="deviceRegModeDisabledRetry" class="primary">Retry</button>
    <button id="deviceRegModeDisabledCancel">Cancel</button>
  </div>
`;

/**
 * Error page which is shown if the identy anchor does not have device registration mode enabled.
 * It shows instructions to the user on how to continue.
 * @param tentativeDeviceInfo Information about the device to be added so that the user does not have to enter everything again after enabling device registration mode.
 */
export const deviceRegistrationDisabledInfo = async (
  tentativeDeviceInfo: TentativeDeviceInfo
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(tentativeDeviceInfo[0]), container);
  return init(tentativeDeviceInfo);
};

const init = async (
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
    await addTentativeDevice(tentativeDeviceInfo);
  };
};
