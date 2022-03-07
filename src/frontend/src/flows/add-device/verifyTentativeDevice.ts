import { html, render } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import { withLoader } from "../../components/loader";
import { renderManage } from "../manage";
import { hasOwnProperty } from "../../utils/utils";
import { displayError } from "../../components/displayError";
import { DeviceData } from "../../../generated/internet_identity_types";
import { Principal } from "@dfinity/principal";
import { toggleErrorMessage } from "../../utils/errorHelper";
import { formatRemainingTime, setupCountdown } from "../../utils/countdown";
import { warningIcon } from "../../components/icons";

const pageContent = (
  alias: string,
  publicKey: string,
  endTimestamp: bigint
) => html`
  <div class="container">
    <h1>Verify New Device</h1>
    <div class="warnBox">
      <div class="warnIcon">${warningIcon}</div>
      <div class="warnContent">
        <div class="warnTitle">Security Warning</div>
        <div class="warnMessage">
          <p>
            This will add the shown device to your Identity Anchor. When
            verified, it will have <b>full control over your identity</b>. Only
            enter a verification code here if you are sure that you
            <i>personally own</i> this device.
          </p>
          <p>
            Enter only codes that were displayed on
            <b>https://identity.ic0.app</b>. Do <b>not</b> enter verification
            codes that you received any other way.
          </p>
        </div>
      </div>
    </div>
    <p>Verify that this is your device:</p>
    <label>Alias</label>
    <div class="highlightBox">${alias}</div>
    <label>Device Verification Code</label>
    <div id="wrongCodeMessage" class="error-message-hidden">
      The entered verification code was invalid. Please try again.
    </div>
    <input id="tentativeDeviceCode" placeholder="Verification Code" />
    <p>
      Time remaining:
      <span id="timer">${formatRemainingTime(endTimestamp)}</span>
    </p>
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
  render(
    pageContent(tentativeDevice.alias, principal, endTimestamp),
    container
  );
  init(userNumber, connection, endTimestamp);
};

const init = (
  userNumber: bigint,
  connection: IIConnection,
  endTimestamp: bigint
) => {
  const countdown = setupCountdown(endTimestamp, () =>
    renderManage(userNumber, connection)
  );
  const cancelButton = document.getElementById(
    "cancelVerifyDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    countdown.stop();
    await withLoader(() => connection.exitDeviceRegistrationMode(userNumber));
    await renderManage(userNumber, connection);
  };

  const pinInput = document.getElementById(
    "tentativeDeviceCode"
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
    if (pinInput.value === "") {
      pinInput.placeholder = "Please enter verification code";
      pinInput.classList.toggle("errored", true);
      return;
    }
    const result = await withLoader(() =>
      connection.verifyTentativeDevice(userNumber, pinInput.value)
    );

    if (hasOwnProperty(result, "verified")) {
      countdown.stop();
      toggleErrorMessage("tentativeDeviceCode", "wrongCodeMessage", false);
      await renderManage(userNumber, connection);
    } else if (
      hasOwnProperty(result, "wrong_code") &&
      result.wrong_code.retries_left > 0
    ) {
      toggleErrorMessage("tentativeDeviceCode", "wrongCodeMessage", true);
    } else {
      await displayError({
        title: "Too Many Wrong Verification Codes Entered",
        message:
          "Adding the device has been aborted due to too many invalid code entries.",
        primaryButton: "Continue",
      });
      await renderManage(userNumber, connection);
    }
  };
};
