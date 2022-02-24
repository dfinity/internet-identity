import { html, render } from "lit-html";
import { parseUserNumber } from "../../utils/userNumber";
import { registerTentativeDevice } from "./registerTentativeDevice";

const pageContent = (userNumber: bigint | null) => html`
  <div class="container">
    <h1>New device</h1>
    <p>
      Please provide the Identity Anchor to which you want to add your device.
    </p>
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

export const addDeviceUserNumber = async (
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
    // TODO L2-309: Try to do this without reload
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
      userNumberInput.classList.toggle("errored", false);
      await registerTentativeDevice(userNumber);
    } else {
      userNumberInput.classList.toggle("errored", true);
      userNumberInput.placeholder = "Please enter your Identity Anchor first";
    }
  };
};
