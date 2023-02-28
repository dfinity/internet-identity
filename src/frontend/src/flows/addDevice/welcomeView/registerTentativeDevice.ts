import { html, render } from "lit-html";
import { creationOptions, Connection } from "../../../utils/iiConnection";
import { WebAuthnIdentity } from "@dfinity/identity";
import { deviceRegistrationDisabledInfo } from "./deviceRegistrationModeDisabled";
import { DerEncodedPublicKey } from "@dfinity/agent";
import {
  KeyType,
  Purpose,
} from "../../../../generated/internet_identity_types";
import { showVerificationCode } from "./showVerificationCode";
import { withLoader } from "../../../components/loader";
import { toggleErrorMessage } from "../../../utils/errorHelper";
import { displayError } from "../../../components/displayError";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = ({ userNumber }: { userNumber: bigint }) => {
  const pageContentSlot = html` <article>
    <hgroup>
      <h1 class="t-title t-title--main">Add a Trusted Device</h1>
      <p class="t-lead">
        What device do you want to add to anchor
        <strong class="t-strong">${userNumber}</strong>?
      </p>
      <p id="invalidAliasMessage" class="is-hidden">
        The device alias must not be empty.
      </p>
    </hgroup>
    <input
      type="text"
      id="tentativeDeviceAlias"
      placeholder="Example: my phone"
      maxlength="64"
      class="c-input"
    />
    <div class="c-button-group">
      <button
        class="c-button c-button--secondary"
        id="registerTentativeDeviceCancel"
      >
        Cancel
      </button>
      <button
        class="c-button"
        id="registerTentativeDeviceContinue"
        class="primary"
      >
        Continue
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};
/**
 * Prompts the user to enter a device alias. When clicking next, the device is added tentatively to the given identity anchor.
 * @param userNumber anchor to add the tentative device to.
 */
export const registerTentativeDevice = async (
  userNumber: bigint,
  connection: Connection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent({ userNumber }), container);
  return init(userNumber, connection);
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
  connection: Connection,
  tentativeDeviceInfo: TentativeDeviceInfo
): Promise<void> => {
  const result = await withLoader(() =>
    connection.addTentativeDevice(...tentativeDeviceInfo)
  );

  if ("added_tentatively" in result) {
    await showVerificationCode(
      tentativeDeviceInfo[0],
      connection,
      tentativeDeviceInfo[1],
      result.added_tentatively,
      Array.from(new Uint8Array(tentativeDeviceInfo[5]))
    );
  } else if ("device_registration_mode_off" in result) {
    await deviceRegistrationDisabledInfo(connection, tentativeDeviceInfo);
  } else if ("another_device_tentatively_added" in result) {
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

const init = async (userNumber: bigint, connection: Connection) => {
  const existingAuthenticators = await withLoader(() =>
    connection.lookupAuthenticators(userNumber)
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
    await addTentativeDevice(connection, tentativeDeviceInfo);
  };
};
