import { html, render } from "lit-html";
import { DeviceData } from "../../../generated/internet_identity_types";
import { mainWindow } from "../../components/mainWindow";

const pageContent = () => {
  const pageContentSlot = html`
    <h1 class="t-title t-title--main">Choose your Recovery Method</h1>
    <p class="t-paragraph t-lead">
      How do you want to recover your Internet Identity?
    </p>
    <div class="l-stack">
      <h2 class="t-title">Recovery devices</h2>
      <ol class="c-list l-stack" id="deviceList"></ol>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

export const pickRecoveryDevice = async (
  devices: DeviceData[]
): Promise<DeviceData> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(devices);
};

export const init = (devices: DeviceData[]): Promise<DeviceData> =>
  new Promise((resolve) => {
    const deviceList = document.getElementById("deviceList") as HTMLElement;
    deviceList.innerHTML = ``;

    const list = document.createElement("ul");

    devices.forEach((device) => {
      const identityElement = document.createElement("li");
      identityElement.className = "deviceItem";
      render(
        html`<li class="deviceItemAlias">
          <button class="c-button c-button--secondary">${device.alias}</button>
        </li>`,
        identityElement
      );
      identityElement.onclick = () => resolve(device);
      list.appendChild(identityElement);
    });
    deviceList.appendChild(list);
  });
