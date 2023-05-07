import { TemplateElement } from "$src/utils/lit-html";
import { nonNullish } from "@dfinity/utils";
import { html, render } from "lit-html";
import { mainWindow } from "./mainWindow";
import { warnBox } from "./warnBox";

export type ErrorOptions = {
  title: TemplateElement;
  message: TemplateElement;
  detail?: TemplateElement;
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

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displayErrorPrimary = document.getElementById(
      "displayErrorPrimary"
    ) as HTMLButtonElement;
    displayErrorPrimary.onclick = () => resolve();
  });
