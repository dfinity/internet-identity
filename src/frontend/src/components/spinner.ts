import { renderPage, TemplateElement } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

export const innerSpinnerTemplate = (): TemplateResult => {
  return html`
    <div class="c-spinner-wrapper">
      <div class="c-spinner">
        <i class="c-spinner__inner"></i>
      </div>
    </div>
  `;
};

export const showSpinnerTemplate = ({
  message,
}: {
  message: TemplateElement;
}): TemplateResult => {
  return html`
    <div
      class="l-container c-card c-card--highlight t-centered c-card--vertically-centered"
    >
      ${innerSpinnerTemplate()}
      <p class="t-lead t-paragraph" style="margin-top: 6rem">${message}</p>
    </div>
  `;
};

export const showSpinnerPage = renderPage(showSpinnerTemplate);

// Show a simple, centered message (with optional "data-role" attribute)
export const showSpinner = ({ message }: { message: TemplateElement }): void =>
  showSpinnerPage({
    message,
  });
