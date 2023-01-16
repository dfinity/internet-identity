import { html, render, TemplateResult } from "lit-html";
import { icLogo } from "./icons";
import { footer } from "./footer";

export type mainWindowProps = {
  slot: TemplateResult;
  showFooter?: boolean;
  showLogo?: boolean;
  isWideContainer?: boolean;
  additionalClasses?: string[];
};

export const mainWindow = ({
  slot,
  showFooter = true,
  showLogo = true,
  isWideContainer = false,
  additionalClasses = [],
}:mainWindowProps):TemplateResult => {
  const containerClasses = ["l-container"];
  if (isWideContainer) {
    containerClasses.push("l-container--wide");
  }
  if (additionalClasses.length > 0) {
    containerClasses.push(...additionalClasses);
  }
  return html`
  <div class="${additionalClasses.join()}">
    ${showLogo ? html`<div class="c-logo">${icLogo}</div>` : ""}
    <div class="c-card c-card--background">
      <div class="c-card c-card--highlight">
        ${slot}
      </div>
      ${showFooter ? footer : ""}
    </div>
  </div>
`};