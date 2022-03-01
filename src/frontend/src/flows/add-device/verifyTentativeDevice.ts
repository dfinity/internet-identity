import { html, render } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { renderManage } from "../manage";
import { hasOwnProperty } from "../../utils/utils";
import { displayError } from "../../components/displayError";
import { DeviceData } from "../../../generated/internet_identity_types";
import { Principal } from "@dfinity/principal";
import { toggleErrorMessage } from "../../utils/errorHelper";
import {formatRemainingTime, setupCountdown} from "../../utils/countdown";

const pageContent = (alias: string, publicKey: string, endTimestamp: bigint) => html`
  <div class="container">
    <h1>Verify New Device</h1>
    <p>Verify that this is your device:</p>
    <label>Alias</label>
    <div class="highlightBox">${alias}</div>
    <label>Public Key</label>
    <div class="highlightBox">${publicKey}</div>
    <div id="wrongPinMessage" class="error-message-hidden">
      The entered PIN was invalid. Please try again.
    </div>
    <label>PIN</label>
    <input id="tentativeDevicePin" placeholder="PIN" />
    <p>Time remaining: <span id="timer">${formatRemainingTime(endTimestamp)}</span></p>
    <button id="verifyDevice" class="primary">Verify Device</button>
    <button id="cancelVerifyDevice" class="linkStyle">Cancel</button>
  </div>
`;

export const verifyDevice = async (
  userNumber: bigint,
  tentativeDevice: DeviceData,
  endTimestamp: bigint,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const principal = Principal.selfAuthenticating(
    new Uint8Array(tentativeDevice.pubkey)
  ).toString();
  render(pageContent(tentativeDevice.alias, principal, endTimestamp), container);
  init(userNumber, connection, endTimestamp);
};

const init = (userNumber: bigint, connection: IIConnection, endTimestamp: bigint) => {
  const countdown = setupCountdown(endTimestamp, () => renderManage(userNumber, connection));
  const cancelButton = document.getElementById(
    "cancelVerifyDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    countdown.stop();
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
      countdown.stop();
      toggleErrorMessage("tentativeDevicePin", "wrongPinMessage", false);
      await renderManage(userNumber, connection);
    } else if (hasOwnProperty(result, "wrong_pin_retry")) {
      toggleErrorMessage("tentativeDevicePin", "wrongPinMessage", true);
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
