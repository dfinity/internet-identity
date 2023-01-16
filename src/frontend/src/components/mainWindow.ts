import { html, render, TemplateResult } from "lit-html";
import { icLogo } from "./icons";
import { footer } from "./footer";

export type mainWindowProps = {
  slot: TemplateResult;
  uid?: string;
  showFooter?: boolean;
  showLogo?: boolean;
  isWideContainer?: boolean;
  additionalContainerClasses?: string[];
  HTMLwrapperTag?: string;
};

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
  uid,
  showFooter = true,
  showLogo = true,
  isWideContainer = false,
  additionalContainerClasses = [],
}:mainWindowProps):TemplateResult => {
  const containerClasses = ["l-container"];
  if (isWideContainer) {
    containerClasses.push("l-container--wide");
  }
  if (additionalContainerClasses.length > 0) {
    containerClasses.push(...additionalContainerClasses);
  }
  return html`
  <div ${uid ? `id=${uid}` : ''} class="${containerClasses.join(' ')}">
    ${showLogo ? html`<div class="c-logo">${icLogo}</div>` : ""}
    <div class="c-card c-card--background">
      <div class="c-card c-card--highlight">
        ${slot}
      </div>
      ${showFooter ? footer : ""}
    </div>
  </div>
`};