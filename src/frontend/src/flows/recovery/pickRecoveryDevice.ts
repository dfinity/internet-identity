import { html, render } from "lit-html";
import { DeviceData } from "../../../generated/internet_identity_types";

const pageContent = () => html`
  <div class="l-container c-card c-card--highlight c-card--highlight">
    <h1>Choose a device</h1>
    <label>Recovery devices</label>
    <div id="deviceList"></div>
  </div>
`;

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
        html`<div class="deviceItemAlias">${device.alias}</div>`,
        identityElement
      );
      identityElement.onclick = () => resolve(device);
      list.appendChild(identityElement);
    });
    deviceList.appendChild(list);
  });
