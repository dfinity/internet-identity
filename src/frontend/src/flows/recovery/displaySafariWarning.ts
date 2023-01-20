import { html, render } from "lit-html";
import { warnBox } from "../../components/warnBox";
import { setupRecovery } from "./setupRecovery";
import { AuthenticatedConnection } from "../../utils/iiConnection";
import { mainWindow } from "../../components/mainWindow";

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

export const displaySafariWarning = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(userNumber, connection);
};

const init = (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> =>
  new Promise((resolve) => {
    const displayWarningAddRecovery = document.getElementById(
      "displayWarningAddRecovery"
    ) as HTMLButtonElement;
    displayWarningAddRecovery.onclick = () => {
      setupRecovery(userNumber, connection).then(() => resolve());
    };
    const displayWarningRemindLater = document.getElementById(
      "displayWarningRemindLater"
    ) as HTMLButtonElement;
    displayWarningRemindLater.onclick = () => {
      resolve();
    };
  });
