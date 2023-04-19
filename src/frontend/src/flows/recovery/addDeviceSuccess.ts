import { html } from "lit-html";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";
import { renderPage } from "../../utils/lit-html";

import { DeviceData } from "../../../generated/internet_identity_types";
import copyJson from "./addDeviceSuccess.json";

export interface AddDeviceSuccessTemplateProps {
  device: DeviceData;
  onContinue: () => void;
  i18n: I18n;
}

const showAddDeviceSuccessTemplate = ({
  device: { alias: deviceAlias },
  onContinue,
  i18n,
}: AddDeviceSuccessTemplateProps) => {
  const { title, continue_to_home, explore } = i18n.i18n(copyJson);

  const slot = html`<article>
    <hgroup>
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-lead">${explore}</p>
    </hgroup>
    <output class="c-input c-input--readonly t-vip t-vip--small"
      >${deviceAlias}</output
    >
    <div class="l-stack">
      <button @click=${onContinue} class="c-button c-button--primary">
        ${continue_to_home}
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot,
  });
};

export const addDeviceSuccessPage = renderPage(showAddDeviceSuccessTemplate);

export const addDeviceSuccess = ({
  device,
}: Pick<AddDeviceSuccessTemplateProps, "device">): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    showAddDeviceSuccessTemplate({
      device,
      onContinue: () => resolve(),
      i18n,
    })
  );
};
