import { mainWindow } from "$lib/templates/mainWindow";
import { I18n } from "$lib/legacy/i18n";
import { renderPage } from "$lib/utils/lit-html";
import { html, render, TemplateResult } from "lit-html";
import copyJson from "./addDeviceSuccess.json";

export type DeviceAlias = string;

export type AddDeviceSuccessTemplateProps = Parameters<
  typeof addDeviceSuccessTemplate
>[0];

const addDeviceSuccessTemplate = ({
  userNumber,
  deviceAlias,
  onContinue,
  stepper,
  i18n,
}: {
  userNumber: bigint;
  deviceAlias: DeviceAlias;
  onContinue: () => void;
  stepper?: TemplateResult;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);

  const slot = html`<article>
    ${stepper}
    <hgroup>
      <div class="c-card__label">
        <h2>${copy.internet_identity} ${userNumber}</h2>
      </div>
      <h1 class="t-title t-title--main">${copy.title}</h1>
      <p class="t-lead">${copy.explore}</p>
    </hgroup>
    <output
      class="c-input c-input--stack c-input--fullwidth c-input--readonly t-vip t-vip--small"
      >${deviceAlias}</output
    >
    <div class="l-stack">
      <button
        @click=${() => onContinue()}
        class="c-button c-button--primary"
        data-action="next"
      >
        ${copy.continue_to_home}
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot,
  });
};

export const addDeviceSuccessPage = renderPage(addDeviceSuccessTemplate);

export const addDeviceSuccess = (
  props: Pick<
    AddDeviceSuccessTemplateProps,
    "userNumber" | "deviceAlias" | "stepper"
  >,
): Promise<void> =>
  new Promise<void>((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(
      addDeviceSuccessTemplate({
        ...props,
        onContinue: resolve,
        i18n: new I18n(),
      }),
      container,
    );
  });
