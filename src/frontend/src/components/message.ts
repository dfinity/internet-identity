import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { renderPage, TemplateElement } from "../utils/lit-html";
import { mainWindow } from "./mainWindow";

const showMessageTemplate = ({
  message,
  role,
}: {
  message: TemplateElement;
  role?: string;
}): TemplateResult =>
  mainWindow({
    showLogo: false,
    showFooter: false,
    slot: html`<p class="t-paragraph t-lead" data-role=${ifDefined(role)}>
      ${message}
    </p>`,
  });

export const showMessagePage = renderPage(showMessageTemplate);

// Show a simple, centered message (with optional "data-role" attribute)
export const showMessage = ({
  message,
  role,
}: {
  message: TemplateElement;
  role?: string;
}): void =>
  showMessagePage({
    message,
    role,
  });
