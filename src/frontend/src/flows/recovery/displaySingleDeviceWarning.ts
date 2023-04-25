import { mainWindow } from "$src/components/mainWindow";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { html, render } from "lit-html";

const pageContent = () => {
  const pageContentSlot = html`
    <article id="warningContainer">
      <h1 class="t-title t-title--main">Add Another Device</h1>
      <p class="t-paragraph">
        We recommend that you have at least two devices (for example, your
        computer and your phone) in case you lose access to one.
      </p>
      <div class="l-stack">
        <button id="displayWarningAddRecovery" class="c-button">
          Add another device
        </button>
        <button
          id="displayWarningRemindLater"
          class="c-button c-button--secondary"
        >
          Skip, I understand the risks
        </button>
      </div>
    </article>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const displaySingleDeviceWarning = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  setupRecovery: (
    userNumber: bigint,
    connection: AuthenticatedConnection
  ) => Promise<void>
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(userNumber, connection, setupRecovery);
};

const init = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  setupRecovery: (
    userNumber: bigint,
    connection: AuthenticatedConnection
  ) => Promise<void>
): Promise<void> =>
  new Promise((resolve) => {
    const displayWarningAddRecovery = document.getElementById(
      "displayWarningAddRecovery"
    ) as HTMLButtonElement;
    displayWarningAddRecovery.onclick = async () => {
      await setupRecovery(userNumber, connection);
      resolve();
    };
    const displayWarningRemindLater = document.getElementById(
      "displayWarningRemindLater"
    ) as HTMLButtonElement;
    displayWarningRemindLater.onclick = () => {
      resolve();
    };
  });
