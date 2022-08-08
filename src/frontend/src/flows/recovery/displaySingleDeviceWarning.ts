import { html, render } from "lit-html";
import { warningIcon } from "../../components/icons";
import { setupRecovery } from "./setupRecovery";
import { IIConnection } from "../../utils/iiConnection";

const pageContent = () => html`
  <article id="warningContainer" class="l-container c-card c-card--highlight">
    <hgroup>
      <div aria-hidden>${warningIcon}</div>
      <h1 id="warningHeading" class="t-title t-title--lead">Warning</h1>
      <p class="t-lead">
        If you lose all the devices assigned to your Internet Identity anchor,
        then you will <em>lose access</em> to the anchor, and all associated
        resources and tokens, unless you have a recovery mechanism setup. This
        can be an external key fob or a secure seedphrase, which you must make
        sure is not stolen.
      </p>
    </hgroup>
    <p class="t-paragraph">
      As a best practice, we recommend you assign multiple devices to an
      Identity Anchor and add <em>at least</em> one recovery mechanism such as
      an external key fob or a seedphrase.
    </p>
    <div class="l-section">
      <button id="displayWarningAddRecovery" class="c-button">
        Add a recovery mechanism to an Identity Anchor
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

export const displaySingleDeviceWarning = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(userNumber, connection);
};

const init = (userNumber: bigint, connection: IIConnection): Promise<void> =>
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
