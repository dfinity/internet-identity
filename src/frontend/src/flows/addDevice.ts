import {
  BinaryBlob,
  blobFromHex,
  derBlobFromBlob,
  DerEncodedBlob,
} from "@dfinity/agent";
import { render, html } from "lit-html";
import { confirm } from "../components/confirm";
import { withLoader } from "../components/loader";
import { initLogout, logoutSection } from "../components/logout";
import { IDPActor } from "../utils/idp_actor";
import { pickDeviceAlias } from "./addDevicePickAlias";
import { successfullyAddedDevice } from "./successfulDeviceAddition";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>New device</h1>
    <label>User Number:</label>
    <div class="highlightBox">${userNumber}</div>
    <p class="warningBox">
      Warning: Do not click this button unless this link really came from you.
    </p>
    <button type="button" class="primary" id="addDevice">Yes, link new device</button>
    <button type="button" id="cancelAdd">Cancel</button>
    ${logoutSection()}
  </div>
`;

export const addDevice = (userNumber: bigint, connection: IDPActor) => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  init(userNumber, connection);
};

const init = (userNumber: bigint, connection: IDPActor) => {
  initLogout();
  const addDeviceButton = document.querySelector(
    "#addDevice"
  ) as HTMLButtonElement;
  const cancelAdd = document.querySelector(
    "#cancelAdd"
  ) as HTMLButtonElement;
  cancelAdd.onclick = () => {
    clearHash();
    window.location.reload()
  }
  addDeviceButton.onclick = async (ev) => {
    // Check URL if user has pasted in an Add Identity link
    const url = new URL(document.URL);
    const parsedParams = parseNewDeviceParam(url.hash?.split("device=")[1]);

    if (!!parsedParams) {
      const { userNumber: expectedUserNumber, publicKey, rawId } = parsedParams;
      if (expectedUserNumber !== userNumber) {
        // Here we're adding a device to our userNumber that was supposed to be added to a different one.
        await confirm({
          message: "Tried adding a device for the wrong user number.",
          detail: `Expected user is ${expectedUserNumber}, but current user is ${userNumber}. Please choose the correct user number when creating the add device link`,
        });
        return;
      }
      console.log("Adding new device with:", parsedParams);
      try {
        const deviceName = await pickDeviceAlias();
        await withLoader(() => connection.add(userNumber, deviceName, publicKey, rawId));
        const container = document.getElementById("pageContent") as HTMLElement;
        clearHash();
        successfullyAddedDevice(deviceName, userNumber, connection);
      } catch (error) {
        // If anything goes wrong, or the user cancels we do _not_ want to add the device.
        await confirm({
          message: "Failed to add the device.",
          detail: `Canceled adding the device with ${error}`,
        });
        clearHash();
        return;
      }
    }
  };
};

const parseNewDeviceParam = (
  param: string
): { userNumber: bigint; publicKey: DerEncodedBlob; rawId?: BinaryBlob } | null => {
  const segments = param.split(";");
  if (!(segments.length === 2 || segments.length === 3)) {
    // TODO: Decent error handling
    console.error("This is not a valid pasted link");
    return null;
  }
  const userNumber = BigInt(segments[0]);
  const publicKey = derBlobFromBlob(blobFromHex(segments[1]));
  const rawId = segments[2] ? blobFromHex(segments[2]) : undefined;
  return { userNumber, publicKey, rawId };
};

export const clearHash = () => {
  history.pushState(
    "",
    document.title,
    window.location.pathname + window.location.search
  );
}
