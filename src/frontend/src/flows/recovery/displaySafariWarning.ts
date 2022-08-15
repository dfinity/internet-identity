import { html, render } from "lit-html";
import { warningIcon } from "../../components/icons";
import { setupRecovery } from "./setupRecovery";
import { AuthenticatedConnection } from "../../utils/iiConnection";

const pageContent = () => html`
  <div id="warningContainer" class="l-container c-car c-card--highlight">
    <hgroup>
      <div aria-hidden>${warningIcon}</div>
      <h1 id="class="t-title t-title--main">Warning</h1>
      <p class="t-lead">It looks like you are using Safari or iOS.</p>
    </hgroup>
    <p class="t-paragraph">
      If you “Clear History and Website Data” via system preferences, all web
      authentication keys will be <em>deleted</em> from this device. This means
      that you will no longer have access to your identity anchor (and all
      associated resources and tokens) with this device.
    </p>
    <p class="t-paragraph">
      As a best practice, we recommend you assign multiple devices to an
      Identity Anchor and add at least one recovery mechanism such as an
      external key fob or a seedphrase
    </p>
    <div class="l-section">
      <button id="displayWarningAddRecovery" class="c-button">
        Add a recovery mechanism to an Identity Anchor
      </button>
      <button id="displayWarningRemindLater" class="c-button c-button--secondary">
        Skip, I understand the risks
      </button>
    </div>
  </div>
`;

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
