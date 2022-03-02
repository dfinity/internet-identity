import { html, render } from "lit-html";
import {
  laptopIcon,
  networkIcon,
  warningIcon,
} from "../../components/icons";

const pageContent = () => html`
  <style>
    .flowChoiceContainer {
      display: flex;
      gap: 1rem;
      margin-top: 1rem;
    }
    .flowOption {
      display: flex;
      flex-direction: column;
      justify-content: flex-start;
      align-items: center;
      border: 1px solid gray;
      border-radius: 4px;
      width: 100%;
      padding: 1rem;
      font-family: "Montserrat", sans-serif;
      font-size: 1.2rem;
      margin-bottom: 2rem;
    }
    .flowOption:hover,
    .flowOption:focus {
      outline: none;
      box-shadow: none;
    }
    .flowIcon {
      height: 52px;
    }
    .flowOptionTitle {
      font-weight: 500;
      margin: 0.5rem;
    }
    .flowOptionDescription {
      text-align: center;
      font-size: 1rem;
    }
  </style>
  <div class="container">
    <h1>Add New Device</h1>
    <div class="warnBox">
      <div class="warnIcon">${warningIcon}</div>
      <div class="warnContent">
        <div class="warnTitle">Security Warning</div>
        <div class="warnMessage">
          <p>
            You are in the process of adding a new device. Any device added here
            will have <b>full control over your identity</b>. Only continue the
            process if you want to add a new device that you
            <i>personally own</i>.
          </p>
          <p>
            <b>
              Do not continue if you were prompted to do this by any other
              website than https://identity.ic0.app!
            </b>
          </p>
        </div>
      </div>
    </div>
    <p>
      Is the device you want to add available on this machine (local device) or a
      different one (remote device)?
    </p>
    <div class="flowChoiceContainer">
      <button class="flowOption" id="local">
        <span class="flowIcon">${laptopIcon}</span>
        <div class="flowOptionTitle">Local Device</div>
        <div class="flowOptionDescription">
          Add a new device available on <i>this machine</i>.
        </div>
      </button>
      <button class="flowOption" id="remote">
        <span class="flowIcon">${networkIcon}</span>
        <div class="flowOptionTitle">Remote Device</div>
        <div class="flowOptionDescription">
          Add a new device available on <i>another machine</i>.
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
    const cancel = document.getElementById(
      "cancelAddDevice"
    ) as HTMLButtonElement;
    localDeviceFlow.onclick = () => resolve("local");
    remoteDeviceFlow.onclick = () => resolve("remote");
    cancel.onclick = () => resolve(null);
  });
