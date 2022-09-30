import { html, render, TemplateResult } from "lit-html";
import { warnBox } from "./warnBox";

export type ErrorOptions = {
  title: string;
  message: string | TemplateResult;
  detail?: string;
  primaryButton: string;
};

const pageContent = (options: ErrorOptions) => html`
  <div id="errorContainer" class="l-container c-card c-card--highlight">
    ${warnBox({
      title: options.title,
      message: options.message,
      htmlElement: "div",
      slot:
        options.detail !== undefined
          ? html`<div class="l-divider"></div>
              <h4>Error details:</h4>
              <pre data-role="error-detail" class="t-paragraph">
${options.detail}</pre
              >`
          : undefined,
    })}

    <div class="l-stack">
      <button id="displayErrorPrimary" class="c-button c-button--primary">
        ${options.primaryButton}
      </button>
    </div>
  </div>
`;

export const displayError = async (options: ErrorOptions): Promise<void> => {
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
