import { html, render, TemplateResult } from "lit-html";
import { warningIcon } from "./icons";

export type ErrorOptions = {
  title: string;
  message: string | TemplateResult;
  detail?: string;
  primaryButton: string;
};

const pageContent = (options: ErrorOptions) => html`
  <div
    id="errorContainer"
    class="l-container c-card c-card--highlight c-card--warning"
  >
    ${warningIcon}
    <hgroup>
      <h1 class="t-title t-title--primary">${options.title}</h1>
      <p class="displayErrorMessage t-lead">${options.message}</p>
    </hgroup>
    ${options.detail !== undefined
      ? html` <details class="displayErrorDetail">
          <summary>Error details</summary>
          <pre>${options.detail}</pre>
        </details>`
      : ""}
    <button id="displayErrorPrimary" class="c-button c-button--primary">
      ${options.primaryButton}
    </button>
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
