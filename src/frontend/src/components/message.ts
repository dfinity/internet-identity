import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { renderPage } from "../utils/lit-html";

const showMessageTemplate = ({
  message,
  role,
}: {
  message: TemplateResult | string;
  role?: string;
}): TemplateResult => {
  return html`<h1
    style="position: absolute; max-width: 100%; top: 50%; transform: translate(0, -50%);"
    data-role=${ifDefined(role)}
  >
    ${message}
  </h1>`;
};

export const showMessagePage = renderPage(showMessageTemplate);

// Show a simple, centered message (with optional "data-role" attribute)
export const showMessage = ({
  message,
  role,
}: {
  message: TemplateResult | string;
  role?: string;
}): void =>
  showMessagePage({
    message,
    role,
  });
