import { ERROR_SUPPORT_URL } from "$lib/config";
import { TemplateElement } from "$lib/utils/lit-html";
import { nonNullish } from "@dfinity/utils";
import { html, render } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { mainWindow } from "./mainWindow";
import { warnBox } from "./warnBox";

export type ErrorOptions = {
  title: TemplateElement;
  message: TemplateElement;
  detail?: TemplateElement;
  errorCode?: string;
  primaryButton: TemplateElement;
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
        slot: nonNullish(options.detail)
          ? html`<div class="l-stack">
              <h4>Error details:</h4>
              <pre data-role="error-detail" class="t-paragraph">
${options.detail}</pre
              >
            </div>`
          : undefined,
      })}

      <div class="l-stack">
        <button
          id="displayErrorPrimary"
          data-error-code=${ifDefined(options.errorCode)}
          class="c-button c-button--primary"
        >
          ${options.primaryButton}
        </button>
        <a
          href="${ERROR_SUPPORT_URL}"
          target="_blank"
          class="c-button c-button--secondary"
        >
          Go to support
        </a>
      </div>`,
  });

export const displayError = (options: ErrorOptions): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(options), container);
  return init();
};

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displayErrorPrimary = document.getElementById(
      "displayErrorPrimary",
    ) as HTMLButtonElement;
    displayErrorPrimary.onclick = () => resolve();
  });
