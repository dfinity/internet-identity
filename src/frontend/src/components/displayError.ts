import { html, render } from "lit-html";
import { TemplateElement } from "../utils/lit-html";
import { mainWindow } from "./mainWindow";
import { warnBox } from "./warnBox";

export type ErrorOptions = {
  title: TemplateElement;
  message: TemplateElement;
  detail?: TemplateElement;
  primaryButton: TemplateElement;
};

export const WEBAUTHN_CANCEL_TEMPLATE = {
  title: "Operation canceled",
  message:
    "The interaction with your security device was canceled or timed out. Please try again.",
};

const pageContent = (options: ErrorOptions) =>
  mainWindow({
    id: "errorContainer",
    showFooter: false,
    showLogo: false,
    slot: html` ${warnBox({
        title: options.title,
        message: options.message,
        htmlElement: "div",
        slot:
          options.detail !== undefined
            ? html`<div class="l-stack">
                <h4>Error details:</h4>
                <pre data-role="error-detail" class="t-paragraph">
${options.detail}</pre
                >
              </div>`
            : undefined,
      })}

      <div class="l-stack">
        <button id="displayErrorPrimary" class="c-button c-button--primary">
          ${options.primaryButton}
        </button>
      </div>`,
  });

export const displayError = (options: ErrorOptions): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(options), container);
  return init();
};

export const displayCancelError = (primaryButton: string) =>
  displayError({
    ...WEBAUTHN_CANCEL_TEMPLATE,
    primaryButton,
  });

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displayErrorPrimary = document.getElementById(
      "displayErrorPrimary"
    ) as HTMLButtonElement;
    displayErrorPrimary.onclick = () => resolve();
  });
