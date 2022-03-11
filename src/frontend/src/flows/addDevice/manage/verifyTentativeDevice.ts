import { html, render } from "lit-html";
import { IIConnection } from "../../../utils/iiConnection";
import { withLoader } from "../../../components/loader";
import { renderManage } from "../../manage";
import { hasOwnProperty } from "../../../utils/utils";
import { displayError } from "../../../components/displayError";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { toggleErrorMessage } from "../../../utils/errorHelper";
import { setupCountdown } from "../../../utils/countdown";
import { warningIcon } from "../../../components/icons";

const pageContent = (alias: string) => html`
  <div class="container">
    <h1>Verify New Device</h1>
    <div class="warnBox">
      <div class="warnIcon">${warningIcon}</div>
      <div class="warnContent">
        <div class="warnTitle">Security Warning</div>
        <div class="warnMessage">
          Verifying will add the shown device to your Identity Anchor. It will
          have <strong>full control over your identity</strong>. Only enter a
          verification code here if you are sure that you
          <em>personally own</em> this device.
        </div>
      </div>
    </div>
    <div class="warnBox">
      <div class="warnIcon">${warningIcon}</div>
      <div class="warnContent">
        <div class="warnTitle">Security Warning</div>
        <div class="warnMessage">
          Enter only codes that were displayed on
          <strong>https://identity.ic0.app</strong>. Do
          <strong>not</strong> enter verification codes that you received any
          other way.
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
    <p>Time remaining: <span id="timer"></span></p>
    <button id="verifyDevice" class="primary">Verify Device</button>
    <button id="cancelVerifyDevice" class="linkStyle">Cancel</button>
  </div>
`;

/**
 * Page to verify the tentative device: the device verification code can be entered and is the checked on the canister.
 * @param userNumber anchor of the authenticated user
 * @param tentativeDevice the tentative device to be verified
 * @param endTimestamp timestamp when the registration mode expires
 * @param connection authenticated II connection
 */
export const verifyDevice = async (
  userNumber: bigint,
  tentativeDevice: DeviceData,
  endTimestamp: bigint,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(tentativeDevice.alias), container);
  init(userNumber, connection, endTimestamp);
};

const init = (
  userNumber: bigint,
  connection: IIConnection,
  endTimestamp: bigint
) => {
  const countdown = setupCountdown(
    endTimestamp,
    document.getElementById("timer") as HTMLElement,
    async () => {
      await displayError({
        title: "Timeout Reached",
        message:
          'The timeout has been reached. For security reasons the "add device" process has been aborted.',
        primaryButton: "Ok",
      });
      await renderManage(userNumber, connection);
    }
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
    } else if (hasOwnProperty(result, "wrong_code")) {
      if (result.wrong_code.retries_left > 0) {
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
    } else if (hasOwnProperty(result, "device_registration_mode_off")) {
      await displayError({
        title: "Device Registration Not Enabled",
        message:
          "Verification not possible because device registration is no longer enabled. Either the timeout has been reached or device registration was disabled using another device.",
        primaryButton: "Continue",
      });
      await renderManage(userNumber, connection);
    } else if (hasOwnProperty(result, "no_device_to_verify")) {
      await displayError({
        title: "No Device To Verify",
        message:
          "Verification not possible because the device is no longer in a state to be verified.",
        primaryButton: "Continue",
      });
      await renderManage(userNumber, connection);
    } else {
      await displayError({
        title: "Something Went Wrong",
        message: "Device could not be verified.",
        detail: JSON.stringify(result),
        primaryButton: "Continue",
      });
    }
  };
};
