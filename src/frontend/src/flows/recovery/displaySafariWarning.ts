import { mainWindow } from "$root/components/mainWindow";
import { warnBox } from "$root/components/warnBox";
import { AuthenticatedConnection } from "$root/utils/iiConnection";
import { html, render } from "lit-html";

const pageContent = () => {
  const pageContentSlot = html` <article id="warningContainer">
    ${warnBox({
      title: "Warning",
      message:
        "You will lose access to your identity anchor if you clear your history and website data on Safari and iOS!",
      slot: html`
        <p class="t-paragraph">
          Make sure you have at least one recovery method.
        </p>
      `,
      htmlElement: "div",
    })}
    <div class="l-stack">
      <button id="displayWarningAddRecovery" class="c-button">
        Add recovery method
      </button>
      <button
        id="displayWarningRemindLater"
        class="c-button c-button--secondary"
      >
        Skip, I understand the risks
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const displaySafariWarning = (
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
