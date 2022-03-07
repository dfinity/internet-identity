import { html, render } from "lit-html";
import {
  addTentativeDevice,
  TentativeDeviceInfo,
} from "./registerTentativeDevice";
import { Principal } from "@dfinity/principal";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Device Registration Disabled</h1>
    <p>
      Device registration is not enabled for the Identity Anchor
      <b>${userNumber}</b>. To enable device registration do the following:
    </p>
    <ol class="instruction-steps">
      <li>
        On an <i>existing</i> device: Log into
        <b>https://identity.ic0.app</b> with Identity Anchor
        <b>${userNumber}</b>
      </li>
      <li>On an <i>existing</i> device: Click <b>Add new device</b></li>
      <li>On an <i>existing</i> device: Chose <b>Remote Device</b></li>
      <li>On <i>this</i> device: Press <b>Retry</b></li>
    </ol>
    <button id="deviceRegModeDisabledRetry" class="primary">Retry</button>
    <button id="deviceRegModeDisabledCancel">Cancel</button>
  </div>
`;

export const deviceRegistrationDisabledInfo = async (
  tentativeDeviceInfo: TentativeDeviceInfo,
  principal: Principal
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(tentativeDeviceInfo[0]), container);
  return init(tentativeDeviceInfo, principal);
};

const init = async (
  tentativeDeviceInfo: TentativeDeviceInfo,
  principal: Principal
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
    await addTentativeDevice(tentativeDeviceInfo, principal);
  };
};
