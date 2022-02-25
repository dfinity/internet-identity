import { html, render } from "lit-html";
import { DeviceData } from "../../../generated/internet_identity_types";
import { securityKeyIcon, seedPhraseIcon } from "../../components/icons";

const pageContent = () => html`
  <style>
    #skipRecovery {
      margin-top: 3.5rem;
      font-weight: 600;
      font-size: 1rem;
    }
    .recoveryContainer {
      display: flex;
      gap: 1rem;
      margin-top: 1rem;
    }
    .recoveryOption {
      display: flex;
      flex-direction: column;
      align-items: center;
      border: 1px solid gray;
      border-radius: 4px;
      width: 100%;
      padding: 1rem;
      font-family: "Montserrat", sans-serif;
      font-size: 1.2rem;
      margin-bottom: 2rem;
    }
    .recoveryOption:disabled:hover,
    .recoveryOption:disabled:focus {
      outline: none;
      box-shadow: none;
    }
    .recoveryIcon {
      height: 52px;
    }
    .recoveryTitle {
      font-weight: 500;
      margin: 0.5rem;
    }
    .recoveryDescription {
      text-align: center;
      font-size: 1rem;
    }
  </style>
  <div class="container">
    <h1>Add New Device</h1>
    <p>
      You can add new authentication devices either by using them on this device
      or by binding a remote device to this identity anchor.
    </p>
    <div class="recoveryContainer">
      <button class="recoveryOption" id="local">
        <span class="recoveryIcon">${seedPhraseIcon}</span>
        <div class="recoveryTitle">Local Device</div>
        <div class="recoveryDescription">
          Add a new authentication mechanism available on
          <i>this device</i> (e.g. YubiKey, Fingerprint, FaceID).
        </div>
      </button>
      <button class="recoveryOption" id="remote">
        <span class="recoveryIcon">${securityKeyIcon}</span>
        <div class="recoveryTitle">Remote Device</div>
        <div class="recoveryDescription">
          Add a new authentication mechanism available on
          <i>another device</i> (e.g. a YubiKey, Fingerprint, FaceID).
        </div>
      </button>
    </div>
    <button id="cancelAddDevice" class="linkStyle">Cancel</button>
  </div>
`;

export type DeviceAddFlow = "local" | "remote";

export const chooseDeviceAddFlow = async (): Promise<DeviceAddFlow | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init();
};

const init = (): Promise<DeviceAddFlow | null> =>
  new Promise((resolve) => {
    const localDeviceFlow = document.getElementById(
      "local"
    ) as HTMLButtonElement;
    const remoteDeviceFlow = document.getElementById(
      "remote"
    ) as HTMLButtonElement;
    const skipRecovery = document.getElementById(
      "cancelAddDevice"
    ) as HTMLButtonElement;
    localDeviceFlow.onclick = () => resolve("local");
    remoteDeviceFlow.onclick = () => resolve("remote");
    skipRecovery.onclick = () => resolve(null);
  });
