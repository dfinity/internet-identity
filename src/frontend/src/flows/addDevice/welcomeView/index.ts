import { html, render } from "lit-html";
import { registerTentativeDevice } from "./registerTentativeDevice";
import { mkAnchorInput } from "../../../components/anchorInput";
import { Connection } from "../../../utils/iiConnection";

const pageContent = (connection: Connection, userNumber?: bigint) => {
  const anchorInput = mkAnchorInput({
    userNumber,
    onSubmit: (userNumber) => registerTentativeDevice(userNumber, connection),
  });

  return html`
    <div class="l-container c-card c-card--highlight">
      <hgroup>
        <h1 class="t-title t-title--main">New Device</h1>
        <p class="t-paragraph t-lead">
          Please provide the Identity Anchor to which you want to add your
          device.
        </p>
        <p id="invalidAnchorMessage" class="is-hidden">
          Please enter a valid Identity Anchor.
        </p>
      </hgroup>
      ${anchorInput.template}
      <div class="c-button-group">
        <button
          @click="${
            () =>
              window.location.reload() /* TODO: L2-309: do this without reload */
          }"
          class="c-button c-button--secondary"
          id="addDeviceUserNumberCancel"
        >
          Cancel
        </button>
        <button
          @click="${anchorInput.submit}"
          class="c-button c-button--primary"
          id="addDeviceUserNumberContinue"
        >
          Continue
        </button>
      </div>
    </div>
  `;
};

/**
 * Entry point for the flow of adding a new authenticator when starting from landing page.
 * This shows a prompt to enter the identity anchor to add this new device to.
 */
export const addRemoteDevice = (
  connection: Connection,
  userNumber?: bigint
): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(connection, userNumber), container);
};
