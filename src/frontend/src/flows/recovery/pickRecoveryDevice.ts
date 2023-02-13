import { html, render } from "lit-html";
import { DeviceData } from "../../../generated/internet_identity_types";
import { mainWindow } from "../../components/mainWindow";
import { securityKeyIcon, seedPhraseIcon } from "../../components/icons";
import { recoveryDeviceToLabel } from "../../utils/recoveryDeviceLabel";

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
  devices: Omit<DeviceData, "alias">[]
): Promise<Omit<DeviceData, "alias">> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(devices);
};

export const init = (
  devices: Omit<DeviceData, "alias">[]
): Promise<Omit<DeviceData, "alias">> =>
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
              >${"seed_phrase" in device.key_type
                ? seedPhraseIcon
                : securityKeyIcon}</span
            >
            <div class="t-strong">${recoveryDeviceToLabel(device)}</div>
          </button>
        </div>`,
        identityElement
      );
      identityElement.onclick = () => resolve(device);
      list.appendChild(identityElement);
    });
    deviceList.appendChild(list);
  });
