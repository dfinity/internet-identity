import { html, render } from "lit-html";
import { parseUserNumber } from "../../../utils/userNumber";
import { registerTentativeDevice } from "./registerTentativeDevice";
import { toggleErrorMessage } from "../../../utils/errorHelper";

const pageContent = (userNumber: bigint | null) => html`
  <div class="container">
    <h1>New Device</h1>
    <p>
      Please provide the Identity Anchor to which you want to add your device.
    </p>
    <div id="invalidAnchorMessage" class="error-message-hidden">
      Please enter a valid Identity Anchor.
    </div>
    <input
      type="text"
      id="addDeviceUserNumber"
      placeholder="Enter Identity Anchor"
      value=${userNumber ?? ""}
    />
    <button id="addDeviceUserNumberContinue" class="primary">Continue</button>
    <button id="addDeviceUserNumberCancel">Cancel</button>
  </div>
`;

/**
 * Entry point for the flow of adding a new authenticator when starting from the welcome view (by clicking 'Already have an anchor but using a new device?').
 * This shows a prompt to enter the identity anchor to add this new device to.
 */
export const addRemoteDevice = async (
  userNumber: bigint | null
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init();
};

const init = () => {
  const cancelButton = document.getElementById(
    "addDeviceUserNumberCancel"
  ) as HTMLButtonElement;

  cancelButton.onclick = () => {
    // TODO L2-309: do this without reload
    window.location.reload();
  };

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
      await registerTentativeDevice(userNumber);
    } else {
      toggleErrorMessage("addDeviceUserNumber", "invalidAnchorMessage", true);
      userNumberInput.placeholder = "Please enter your Identity Anchor first";
    }
  };
};
