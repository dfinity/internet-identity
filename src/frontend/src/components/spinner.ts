import { spinner } from "$src/components/icons";
import { renderPage, TemplateElement } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

const showSpinnerTemplate = ({
  message,
}: {
  message: TemplateElement;
}): TemplateResult => {
  return html`
    <div
      class="l-container c-card c-card--highlight t-centered c-card--vertically-centered"
    >
      <div class="c-spinner-wrapper">
        <div class="c-spinner">${spinner}</div>
      </div>
      <p class="t-lead t-paragraph l-stack">${message}</p>
    </div>
  `;
};

export const showSpinnerPage = renderPage(showSpinnerTemplate);

// Show a simple, centered message (with optional "data-role" attribute)
export const showSpinner = ({ message }: { message: TemplateElement }): void =>
  showSpinnerPage({
    message,
  });
