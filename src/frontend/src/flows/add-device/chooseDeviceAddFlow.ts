import { html, render } from "lit-html";
import { securityKeyIcon} from "../../components/icons";

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
    <p>
      Is the device you want to add attached to the machine your currently using?
    </p>
    <div class="flowChoiceContainer">
      <button class="flowOption" id="local">
        <span class="flowIcon">${securityKeyIcon}</span>
        <div class="flowOptionTitle">Yes: Local Device</div>
        <div class="flowOptionDescription">
          Add a new device attached to <i>this machine</i>.
        </div>
      </button>
      <button class="flowOption" id="remote">
        <span class="flowIcon">${securityKeyIcon}</span>
        <div class="flowOptionTitle">No: Remote Device</div>
        <div class="flowOptionDescription">
          Add a new device attached to <i>another machine</i>.
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
