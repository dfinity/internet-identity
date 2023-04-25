import { mainWindow } from "$root/components/mainWindow";
import { I18n } from "$root/i18n";
import { renderPage } from "$root/utils/lit-html";
import { html, render } from "lit-html";
import copyJson from "./addDeviceSuccess.json";

export type DeviceAlias = string;

export type AddDeviceSuccessTemplateProps = Parameters<
  typeof addDeviceSuccessTemplate
>[0];

const addDeviceSuccessTemplate = ({
  deviceAlias,
  onContinue,
  i18n,
}: {
  deviceAlias: DeviceAlias;
  onContinue: () => void;
  i18n: I18n;
}) => {
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
      <button
        @click=${() => onContinue()}
        class="c-button c-button--primary"
        data-action="next"
      >
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

export const addDeviceSuccessPage = renderPage(addDeviceSuccessTemplate);

export const renderAddDeviceSuccess = (
  props: Pick<AddDeviceSuccessTemplateProps, "deviceAlias">
): Promise<void> =>
  new Promise<void>((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(
      addDeviceSuccessTemplate({
        ...props,
        onContinue: resolve,
        i18n: new I18n(),
      }),
      container
    );
  });
