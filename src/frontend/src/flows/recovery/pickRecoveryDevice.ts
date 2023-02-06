import { html, render } from "lit-html";
import { DeviceData } from "@generated/internet_identity_types";
import { mainWindow } from "../../components/mainWindow";
import { securityKeyIcon, seedPhraseIcon } from "../../components/icons";

const pageContent = () => {
  const pageContentSlot = html`
    <hgroup>
      <h1 class="t-title t-title--main">Choose Recovery Method</h1>
      <p class="t-paragraph t-lead">
        How do you want to recover your Internet Identity?
      </p>
    </hgroup>
    <div id="deviceList"></div>
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
    list.classList.add("l-horizontal", "l-stack--tight");

    devices.forEach((device) => {
      const identityElement = document.createElement("li");
      identityElement.className = "deviceItem";
      render(
        html`<div class="deviceItemAlias">
          <button class="c-button c-button--secondary">
            <span aria-hidden="true"
              >${device.alias === "Recovery Phrase"
                ? seedPhraseIcon
                : securityKeyIcon}</span
            >
            <div class="t-strong">${device.alias}</div>
          </button>
        </div>`,
        identityElement
      );
      identityElement.onclick = () => resolve(device);
      list.appendChild(identityElement);
    });
    deviceList.appendChild(list);
  });
