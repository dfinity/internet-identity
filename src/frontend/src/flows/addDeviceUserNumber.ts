import { blobFromUint8Array, blobToHex, derBlobFromBlob } from "@dfinity/agent";
import { WebAuthnIdentity } from "@dfinity/identity";
import { html, render } from "lit-html";
import { nextTick } from "process";
import { IDPActor } from "../utils/idp_actor";
import { parseUserNumber, setUserNumber } from "../utils/userNumber";
import { displayAddDeviceLink } from "./displayAddDeviceLink";

const pageContent = (userNumber: bigint | null) => html`
  <div class="container">
    <h1>New device</h1>
    <p>Please provide your user number.</p>
    <form id="addDeviceUserNumberForm">
      <input
        type="text"
        id="addDeviceUserNumber"
        placeholder="Enter User Number"
        value=${userNumber ?? ""}
      />
      <button id="addDeviceUserNumberContinue" class="primary" type="submit">
        Continue
      </button>
    </form>
  </div>
`;

export const addDeviceUserNumber = async (
  userNumber: bigint | null
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  await nextTick(() => {
    (document.getElementById("addDeviceUserNumber") as
      | HTMLInputElement
      | undefined)?.focus();
  });
  return init();
};

const init = () => {
  const addDeviceUserNumberForm = document.getElementById(
    "addDeviceUserNumberForm"
  ) as HTMLFormElement;
  addDeviceUserNumberForm.onsubmit = async (e) => {
    e.preventDefault();
    const userNumberInput = document.getElementById(
      "addDeviceUserNumber"
    ) as HTMLInputElement;
    let loginInterval: number;

    const userNumber = parseUserNumber(userNumberInput.value);
    if (userNumber !== null) {
      userNumberInput.classList.toggle("errored", false);
      const identity = await WebAuthnIdentity.create();
      const publicKey = identity.getPublicKey().toDer();
      const rawId = blobToHex(identity.rawId);

      let url = new URL(location.toString());
      url.pathname = "/";
      url.hash = `#device=${userNumber};${blobToHex(publicKey)};${rawId}`;
      const link = encodeURI(url.toString());

      displayAddDeviceLink(link);
      loginInterval = window.setInterval(async () => {
        console.log("checking if authenticated");
        try {
          let devices = await IDPActor.lookup(userNumber);
          let matchedDevice = devices.find((deviceData) =>
            derBlobFromBlob(
              blobFromUint8Array(Buffer.from(deviceData.pubkey))
            ).equals(publicKey)
          );
          if (matchedDevice !== undefined) {
            window.clearInterval(loginInterval);
            setUserNumber(userNumber);
            window.location.reload();
          }
        } catch (error) {
          console.error(error);
        }
      }, 2500);
    } else {
      userNumberInput.classList.toggle("errored", true);
      userNumberInput.placeholder = "Please enter your User Number first";
    }
    return false;
  };
};
