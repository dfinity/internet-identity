import { html, render, TemplateResult } from "lit";
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
      slot:
        options.detail !== undefined
          ? html`<details class="displayErrorDetail">
              <summary class="c-summary">
                <span class="c-summary__link t-link">Error details</span>
              </summary>
              <pre class="t-paragraph">${options.detail}</pre>
            </details>`
          : undefined,

      htmlElement: "div",
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
