import { html, render } from "lit-html";
import { parseUserNumber } from "../../../utils/userNumber";
import { registerTentativeDevice } from "./registerTentativeDevice";
import { toggleErrorMessage } from "../../../utils/errorHelper";
import { mkAnchorInput } from "../../../components/anchorInput";
import { Connection } from "../../../utils/iiConnection";

const pageContent = (userNumber?: bigint) => html`
  <div class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">New Device</h1>
      <p class="t-paragraph t-lead">
        Please provide the Identity Anchor to which you want to add your device.
      </p>
      <p id="invalidAnchorMessage" class="is-hidden">
        Please enter a valid Identity Anchor.
      </p>
    </hgroup>
    ${mkAnchorInput("addDeviceUserNumber", userNumber).template}
    <div class="c-button-group">
      <button
        @click="${
          window.location.reload /* TODO: L2-309: do this without reload */
        }"
        class="c-button c-button--secondary"
        id="addDeviceUserNumberCancel"
      >
        Cancel
      </button>
      <button
        class="c-button c-button--primary"
        id="addDeviceUserNumberContinue"
      >
        Continue
      </button>
    </div>
  </div>
`;

/**
 * Entry point for the flow of adding a new authenticator when starting from the welcome view (by clicking 'Already have an anchor but using a new device?').
 * This shows a prompt to enter the identity anchor to add this new device to.
 */
export const addRemoteDevice = async (
  connection: Connection,
  userNumber?: bigint
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(connection);
};

const init = (connection: Connection) => {
  const continueButton = document.getElementById(
    "addDeviceUserNumberContinue"
  ) as HTMLButtonElement;
  const userNumberInput = document.getElementById(
    "addDeviceUserNumber"
  ) as HTMLInputElement;

  userNumberInput.onkeypress = (e) => {
    // submit if user hits enter
    if (e.key === "Enter") {
      e.preventDefault();
      continueButton.click();
    }
  };

  continueButton.onclick = async () => {
    const userNumber = parseUserNumber(userNumberInput.value);
    if (userNumber !== null) {
      toggleErrorMessage("addDeviceUserNumber", "invalidAnchorMessage", false);
      await registerTentativeDevice(userNumber, connection);
    } else {
      toggleErrorMessage("addDeviceUserNumber", "invalidAnchorMessage", true);
      userNumberInput.placeholder = "Please enter your Identity Anchor first";
    }
  };
};
