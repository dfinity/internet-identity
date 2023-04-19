import { html } from "lit-html";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";
import { renderPage } from "../../utils/lit-html";

import copyJson from "./addDeviceSuccess.json";

const showAddDeviceSuccessTemplate = ({
  onContinue,
  i18n,
}: {
  onContinue: () => void;
  i18n: I18n;
}) => {
  const { title, continue_to_home, explore } = i18n.i18n(copyJson);

  const slot = html`<hgroup>
    <h1 class="t-title t-title--main">${title}</h1>
    <p class="t-lead">${explore}</p>
    <div class="l-stack">
      <div class="l-stack">
        <button @click=${onContinue} class="c-button c-button--primary">
          ${continue_to_home}
        </button>
      </div>
    </div>
  </hgroup>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot,
  });
};

export const addDeviceSuccessPage = renderPage(showAddDeviceSuccessTemplate);

export const addDeviceSuccess = (): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    showAddDeviceSuccessTemplate({
      onContinue: () => resolve(),
      i18n,
    })
  );
};
