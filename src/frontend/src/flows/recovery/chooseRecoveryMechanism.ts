import { html, render } from "lit-html";
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
    <h1>Recovery Options</h1>
    <p>Set up account recovery to protect your Internet Identity.</p>
    <div class="recoveryContainer">
      <button class="recoveryOption" id="seedPhrase">
        <span class="recoveryIcon">${seedPhraseIcon}</span>
        <div class="recoveryTitle">Seedphrase</div>
        <div class="recoveryDescription">Use your own storage</div>
      </button>
      <button class="recoveryOption" id="securityKey">
        <span class="recoveryIcon">${securityKeyIcon}</span>
        <div class="recoveryTitle">Security Key</div>
        <div class="recoveryDescription">Use if you own a security key</div>
      </button>
    </div>
    <button id="skipRecovery" class="linkStyle">Set recovery later</button>
  </div>
`;

export type RecoveryMechanism = "securityKey" | "seedPhrase";

export const chooseRecoveryMechanism = async (): Promise<RecoveryMechanism | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
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
