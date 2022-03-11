import { html, render } from "lit-html";
import { creationOptions, IIConnection } from "../../../utils/iiConnection";
import { WebAuthnIdentity } from "@dfinity/identity";
import { deviceRegistrationDisabledInfo } from "./deviceRegistrationModeDisabled";
import { DerEncodedPublicKey } from "@dfinity/agent";
import {
  KeyType,
  Purpose,
} from "../../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../../utils/utils";
import { showVerificationCode } from "./showVerificationCode";
import { withLoader } from "../../../components/loader";
import { toggleErrorMessage } from "../../../utils/errorHelper";
import { displayError } from "../../../components/displayError";

const pageContent = () => html`
  <div class="container">
    <h1>New Device</h1>
    <p>Please provide an alias for this device.</p>
    <div id="invalidAliasMessage" class="error-message-hidden">
      The device alias must not be empty.
    </div>
    <input
      type="text"
      id="tentativeDeviceAlias"
      placeholder="Device Alias"
      maxlength="64"
    />
    <button id="registerTentativeDeviceContinue" class="primary">
      Continue
    </button>
    <button id="registerTentativeDeviceCancel">Cancel</button>
  </div>
`;

/**
 * Prompts the user to enter a device alias. When clicking next, the device is added tentatively to the given identity anchor.
 * @param userNumber anchor to add the tentative device to.
 */
export const registerTentativeDevice = async (
  userNumber: bigint
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(userNumber);
};

export type TentativeDeviceInfo = [
  bigint,
  string,
  KeyType,
  Purpose,
  DerEncodedPublicKey,
  ArrayBuffer
];

export const addTentativeDevice = async (
  tentativeDeviceInfo: TentativeDeviceInfo
): Promise<void> => {
  const result = await withLoader(() =>
    IIConnection.addTentativeDevice(...tentativeDeviceInfo)
  );

  if (hasOwnProperty(result, "added_tentatively")) {
    await showVerificationCode(
      tentativeDeviceInfo[0],
      tentativeDeviceInfo[1],
      result.added_tentatively,
      Array.from(new Uint8Array(tentativeDeviceInfo[5]))
    );
  } else if (hasOwnProperty(result, "device_registration_mode_off")) {
    await deviceRegistrationDisabledInfo(tentativeDeviceInfo);
  } else if (hasOwnProperty(result, "another_device_tentatively_added")) {
    await displayError({
      title: "Tentative Device Already Exists",
      message:
        'The "add device" process was already started for another device. If you want to add this device instead, log in using an existing device and restart the "add device" process.',
      primaryButton: "Ok",
    });
    // TODO L2-309: do this without reload
    window.location.reload();
  } else {
    throw new Error(
      "unknown tentative device registration result: " + JSON.stringify(result)
    );
  }
};

const init = async (userNumber: bigint) => {
  const existingAuthenticators = await withLoader(() =>
    IIConnection.lookupAuthenticators(userNumber)
  );
  const cancelButton = document.getElementById(
    "registerTentativeDeviceCancel"
  ) as HTMLButtonElement;

  cancelButton.onclick = () => {
    // TODO L2-309: do this without reload
    window.location.reload();
  };

  const continueButton = document.getElementById(
    "registerTentativeDeviceContinue"
  ) as HTMLButtonElement;
  const aliasInput = document.getElementById(
    "tentativeDeviceAlias"
  ) as HTMLInputElement;

  aliasInput.onkeypress = (e) => {
    // submit if user hits enter
    if (e.key === "Enter") {
      e.preventDefault();
      continueButton.click();
    }
  };

  continueButton.onclick = async () => {
    if (aliasInput.value === "") {
      toggleErrorMessage("tentativeDeviceAlias", "invalidAliasMessage", true);
      return;
    }

    let newDevice: WebAuthnIdentity;
    try {
      newDevice = await WebAuthnIdentity.create({
        publicKey: creationOptions(existingAuthenticators),
      });
    } catch (error: unknown) {
      await displayError({
        title: "Error adding new device",
        message: "Unable to register new WebAuthn Device.",
        detail: error instanceof Error ? error.message : JSON.stringify(error),
        primaryButton: "Ok",
      });
      // TODO L2-309: do this without reload
      window.location.reload();
      return;
    }
    const tentativeDeviceInfo: TentativeDeviceInfo = [
      userNumber,
      aliasInput.value,
      { unknown: null },
      { authentication: null },
      newDevice.getPublicKey().toDer(),
      newDevice.rawId,
    ];
    await addTentativeDevice(tentativeDeviceInfo);
  };
};
