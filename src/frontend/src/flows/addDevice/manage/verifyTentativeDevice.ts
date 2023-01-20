import { html, render } from "lit-html";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { withLoader } from "../../../components/loader";
import { renderManage } from "../../manage";
import { displayError } from "../../../components/displayError";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { toggleErrorMessage } from "../../../utils/errorHelper";
import { setupCountdown } from "../../../utils/countdown";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = (alias: string) => {
  const pageContentSlot = html`<h1 class="t-title t-title--main">
      Do you trust this device with your Identity Anchor?
    </h1>
    <output class="c-input c-input--readonly t-vip t-vip--small"
      >${alias}</output
    >
    <p class="t-paragraph">
      If you trust this device to use and manage your Internet Identity, enter
      the <strong class="t-strong">Verification Code</strong> displayed on your
      new device:
    </p>
    <label class="l-stack" aria-label="Verification Code">
      <p id="wrongCodeMessage" class="is-hidden t-paragraph">
        The entered verification code was invalid. Please try again.
      </p>
      <input
        id="tentativeDeviceCode"
        class="c-input"
        placeholder="Verification Code"
      />
    </label>
    <p class="t-paragraph">
      Time remaining: <span id="timer" class="t-strong"></span>
    </p>

    <div class="l-stack">
      <button id="verifyDevice" class="c-button">Verify Device</button>
      <button id="cancelVerifyDevice" class="c-button c-button--secondary">
        Cancel
      </button>
    </div>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

/**
 * Page to verify the tentative device: the device verification code can be entered and is the checked on the canister.
 * @param userNumber anchor of the authenticated user
 * @param tentativeDevice the tentative device to be verified
 * @param endTimestamp timestamp when the registration mode expires
 * @param connection authenticated II connection
 */
export const verifyDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  tentativeDevice: DeviceData,
  endTimestamp: bigint
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(tentativeDevice.alias), container);
  init(userNumber, connection, endTimestamp);
};

const init = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
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
    await withLoader(() => connection.exitDeviceRegistrationMode());
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
      pinInput.classList.toggle("has-error", true);
      return;
    }
    const result = await withLoader(() =>
      connection.verifyTentativeDevice(pinInput.value)
    );

    if ("verified" in result) {
      countdown.stop();
      toggleErrorMessage("tentativeDeviceCode", "wrongCodeMessage", false);
      await renderManage(userNumber, connection);
    } else if ("wrong_code" in result) {
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
    } else if ("device_registration_mode_off" in result) {
      await displayError({
        title: "Device Registration Not Enabled",
        message:
          "Verification not possible because device registration is no longer enabled. Either the timeout has been reached or device registration was disabled using another device.",
        primaryButton: "Continue",
      });
      await renderManage(userNumber, connection);
    } else if ("no_device_to_verify" in result) {
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
