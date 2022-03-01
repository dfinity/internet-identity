import { html, render } from "lit-html";
import { creationOptions, IIConnection } from "../../utils/iiConnection";
import { WebAuthnIdentity } from "@dfinity/identity";
import { deviceRegistrationDisabledInfo } from "./deviceRegistrationModeDisabled";
import { DerEncodedPublicKey } from "@dfinity/agent";
import { KeyType, Purpose } from "../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../utils/utils";
import { showPin } from "./showPin";
import { withLoader } from "../../components/loader";
import { Principal } from "@dfinity/principal";
import { toggleErrorMessage } from "../../utils/errorHelper";
import { displayError } from "../../components/displayError";

const pageContent = () => html`
  <div class="container">
    <h1>New device</h1>
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
  tentativeDeviceInfo: TentativeDeviceInfo,
  principal: Principal
): Promise<void> => {
  const result = await withLoader(() =>
    IIConnection.addTentativeDevice(...tentativeDeviceInfo)
  );

  if (hasOwnProperty(result, "added_tentatively")) {
    await showPin(
      tentativeDeviceInfo[0],
      tentativeDeviceInfo[1],
      principal,
      result.added_tentatively,
      Array.from(new Uint8Array(tentativeDeviceInfo[5]))
    );
  } else if (hasOwnProperty(result, "device_registration_mode_disabled")) {
    await deviceRegistrationDisabledInfo(tentativeDeviceInfo, principal);
  } else if (hasOwnProperty(result, "tentative_device_already_exists")) {
    console.log("tentative_device_already_exists");
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
    // TODO L2-309: Try to do this without reload
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
      // TODO L2-309: Try to do this without reload
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
    await addTentativeDevice(tentativeDeviceInfo, newDevice.getPrincipal());
  };
};
