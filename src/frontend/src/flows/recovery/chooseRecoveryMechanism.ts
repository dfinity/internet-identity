import { html, render } from "lit-html";
import { DeviceData } from "../../../generated/internet_identity_types";
import { securityKeyIcon, seedPhraseIcon } from "../../components/icons";
import { mainWindow } from "../../components/mainWindow";

const pageContent = (devices: DeviceData[]) => {
  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">Choose a Recovery Method</h1>
        <p class="t-lead">
          We recommend that you create at least one recovery method in case you
          lose access to your devices.
        </p>
      </hgroup>
      <div class="l-horizontal l-stack">
        <button
          ?disabled=${hasRecoveryPhrase(devices)}
          class="c-button c-button--secondary"
          id="seedPhrase"
        >
          <span aria-hidden="true">${seedPhraseIcon}</span>
          <div class="t-strong">Recovery Phrase</div>
          <div class="t-weak">Use your own storage</div>
        </button>
        <button
          ?disabled=${hasRecoveryKey(devices)}
          class="c-button c-button--secondary"
          id="securityKey"
        >
          <span aria-hidden="true">${securityKeyIcon}</span>
          <div class="t-strong">External Hardware</div>
          <div class="t-weak">Use an extra security key</div>
        </button>
      </div>
      <div class="l-stack">
        <button id="skipRecovery" class="c-button">
          Skip, I understand the risks
        </button>
      </div>
    </article>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

export type RecoveryMechanism = "securityKey" | "seedPhrase";

export const chooseRecoveryMechanism = async (
  devices: DeviceData[]
): Promise<RecoveryMechanism | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(devices), container);
  return init();
};

const init = (): Promise<RecoveryMechanism | null> =>
  new Promise((resolve) => {
    const securityKey = document.getElementById(
      "securityKey"
    ) as HTMLButtonElement;
    const seedPhrase = document.getElementById(
      "seedPhrase"
    ) as HTMLButtonElement;
    const skipRecovery = document.getElementById(
      "skipRecovery"
    ) as HTMLButtonElement;
    securityKey.onclick = () => resolve("securityKey");
    seedPhrase.onclick = () => resolve("seedPhrase");
    skipRecovery.onclick = () => resolve(null);
  });

const hasRecoveryPhrase = (devices: DeviceData[]): boolean =>
  devices.some((device) => device.alias === "Recovery phrase");
const hasRecoveryKey = (devices: DeviceData[]): boolean =>
  devices.some((device) => device.alias === "Recovery key");
