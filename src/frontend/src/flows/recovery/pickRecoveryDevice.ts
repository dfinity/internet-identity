import { securityKeyIcon, seedPhraseIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import {
  isRecoveryPhrase,
  RecoveryDevice,
  recoveryDeviceToLabel,
} from "$src/utils/recoveryDevice";
import { html, render } from "lit-html";

const pageContent = () => {
  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">Choose Recovery Method</h1>
        <p class="t-paragraph t-lead">
          How do you want to recover your Internet Identity?
        </p>
      </hgroup>
      <div class="l-horizontal l-stack" id="deviceList"></div>
    </article>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

export const pickRecoveryDevice = (
  devices: RecoveryDevice[]
): Promise<RecoveryDevice> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(devices);
};

export const init = (devices: RecoveryDevice[]): Promise<RecoveryDevice> =>
  new Promise((resolve) => {
    const deviceList = document.getElementById("deviceList") as HTMLElement;
    deviceList.innerHTML = ``;

    devices.forEach((device) => {
      const btn = document.createElement("button");
      btn.classList.add("c-button", "c-button--secondary");
      render(
        html`
          <span aria-hidden="true"
            >${isRecoveryPhrase(device)
              ? seedPhraseIcon
              : securityKeyIcon}</span
          >
          <div class="t-strong">${recoveryDeviceToLabel(device)}</div>
        `,
        btn
      );
      btn.onclick = () => resolve(device);
      deviceList.appendChild(btn);
    });
  });
