import { mainWindow } from "$lib/templates/mainWindow";
import { tentativeDeviceStepper } from "$lib/legacy/flows/addDevice/stepper";
import copyJson from "$lib/legacy/flows/addDevice/welcomeView/promptDeviceTrusted.json";
import { I18n } from "$lib/legacy/i18n";
import { renderPage } from "$lib/utils/lit-html";
import { html } from "lit-html";

export type PromptDeviceTrustedTemplateProps = Parameters<
  typeof promptDeviceTrustedTemplate
>[0];

const promptDeviceTrustedTemplate = ({
  userNumber,
  confirm,
  cancel,
  i18n,
}: {
  userNumber: bigint;
  confirm: () => void;
  cancel: () => void;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);

  const pageContentSlot = html`<article>
    ${tentativeDeviceStepper({ step: "activate" })}
    <hgroup>
      <div class="c-card__label">
        <h2>${copy.internet_identity} ${userNumber}</h2>
      </div>
      <h1 class="t-title t-title--main">${copy.activate_passkey}</h1>
    </hgroup>
    <p class="t-paragraph">${copy.trust_this_device}</p>
    <div class="l-stack">
      <button
        id="trustDeviceConfirm"
        class="c-button"
        @click=${() => confirm()}
      >
        ${copy.yes}
      </button>
      <button
        id="trustDeviceCancel"
        class="c-button c-button--secondary"
        @click=${() => cancel()}
      >
        ${copy.no}
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const promptDeviceTrustedPage = renderPage(promptDeviceTrustedTemplate);

/**
 * Page to prompt the user whether they trust the current device.
 */
export const promptDeviceTrusted = (
  props: Pick<PromptDeviceTrustedTemplateProps, "userNumber">,
): Promise<"confirmed" | "canceled"> => {
  return new Promise((resolve) =>
    promptDeviceTrustedPage({
      ...props,
      confirm: () => resolve("confirmed"),
      cancel: () => resolve("canceled"),
      i18n: new I18n(),
    }),
  );
};
