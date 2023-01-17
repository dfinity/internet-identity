import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit/directives/if-defined.js";
import { icLogo } from "./icons";
import { footer } from "./footer";

/**
 * main window template
 * @argument slot - HTML content of the main window
 * @argument showFooter - show footer or not
 * @argument showLogo - show logo or not
 * @argument isWideContainer - some pages need a wider container (FAQ, etc.)
 * @argument additionalContainerClasses - additional classes to add to the container
 * @returns TemplateResult
 */
export const mainWindow = ({
  slot,
  id,
  showFooter = true,
  showLogo = true,
  isWideContainer = false,
  additionalContainerClasses = [],
}: {
  slot: TemplateResult;
  id?: string;
  showFooter?: boolean;
  showLogo?: boolean;
  isWideContainer?: boolean;
  additionalContainerClasses?: string[];
  HTMLwrapperTag?: string;
}): TemplateResult => {
  const containerClasses = ["l-container"];
  if (isWideContainer === true) {
    containerClasses.push("l-container--wide");
  }
  if (additionalContainerClasses.length > 0) {
    containerClasses.push(...additionalContainerClasses);
  }
  return html`
    <div
      id="${ifDefined(id !== null ? id : undefined)}"
      class="${containerClasses.join(" ")}"
    >
      ${showLogo ? html`<div class="c-logo">${icLogo}</div>` : ""}
      <div class="c-card c-card--background">
        <div class="c-card c-card--highlight">${slot}</div>
        ${showFooter ? footer : ""}
      </div>
    </div>
  `;
};
