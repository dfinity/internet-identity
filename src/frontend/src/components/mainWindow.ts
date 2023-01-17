import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit/directives/if-defined.js";
import { icLogo } from "./icons";
import { footer } from "./footer";

/**
 * main window template
 *
 * To avoid having to repeat the same structure in every page,
 * we use this component to wrap the content of each page.
 * This way, we can change the structure of the main window
 * in one place.
 *
 * It is a component that includes the logo, the footer, and the container.
 *
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
