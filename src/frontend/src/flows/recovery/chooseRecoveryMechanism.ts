import { html, render } from "lit-html";

const pageContent = () => html`
  <style>
  #skipRecovery {
    margin-top: 3rem;
    font-weight: 600;
    font-size: 0.9rem;
  }
  </style>
  <div class="container">
    <h1>Choose a recovery mechanism</h1>
    <p>Set up account recovery to protect your Internet Identity.</p>
    <button id="securityKey"">Additional security key</button>
    <button id="seedPhrase"">Recovery password</button>
    <button id="skipRecovery" class="linkStyle">
      Do it later and risk losing access
    </button>
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
