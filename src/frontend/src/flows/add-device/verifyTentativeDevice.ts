import { html, render } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { renderManage } from "../manage";
import { hasOwnProperty } from "../../utils/utils";
import { displayError } from "../../components/displayError";
import { DeviceData } from "../../../generated/internet_identity_types";
import { Principal } from "@dfinity/principal";

const pageContent = (alias: string, publicKey: string) => html`
  <div class="container">
    <h1>Verify New Device</h1>
    <p>Verify that this is your device:</p>
    <label>Alias</label>
    <div class="highlightBox">${alias}</div>
    <label>Public Key</label>
    <div class="highlightBox">${publicKey}</div>
    <input id="tentativeDevicePin" placeholder="PIN" type="password" />
    <button id="verifyDevice" class="primary">Verify Device</button>
    <button id="cancelVerifyDevice" class="linkStyle">Cancel</button>
  </div>
`;

export const verifyDevice = async (
  userNumber: bigint,
  tentativeDevice: DeviceData,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const principal = Principal.selfAuthenticating(
    new Uint8Array(tentativeDevice.pubkey)
  ).toString();
  render(pageContent(tentativeDevice.alias, principal), container);

  init(userNumber, connection);
};

const init = (userNumber: bigint, connection: IIConnection) => {
  const cancelButton = document.getElementById(
    "cancelVerifyDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    await withLoader(() =>
      connection.disableDeviceRegistrationMode(userNumber)
    );
    await renderManage(userNumber, connection);
  };

  const pinInput = document.getElementById(
    "tentativeDevicePin"
  ) as HTMLInputElement;
  const verifyButton = document.getElementById(
    "verifyDevice"
  ) as HTMLButtonElement;

  pinInput.onkeypress = (e) => {
    // submit if user hits enter
    if (e.key === "Enter") {
      e.preventDefault();
      verifyButton.click();
    }
  };

  verifyButton.onclick = async () => {
    const result = await withLoader(() =>
      connection.verifyTentativeDevice(userNumber, pinInput.value)
    );

    if (hasOwnProperty(result, "verified")) {
      pinInput.classList.toggle("errored", false);
      await renderManage(userNumber, connection);
    } else if (hasOwnProperty(result, "wrong_pin_retry")) {
      pinInput.classList.toggle("errored", true);
    } else if (hasOwnProperty(result, "wrong_pin")) {
      await displayError({
        title: "Too Many Wrong Pins Entered",
        message:
          "Adding the device has been aborted due to too many invalid pin entries.",
        primaryButton: "Continue",
      });
      await renderManage(userNumber, connection);
    }
  };
};
