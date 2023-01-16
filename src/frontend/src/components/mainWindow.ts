import { html, render, TemplateResult } from "lit-html";
import { icLogo } from "./icons";
import { footer } from "./footer";

export type mainWindowProps = {
  slot: TemplateResult;
  showFooter?: boolean;
  showLogo?: boolean;
};

export const mainWindow = ({
  slot,
  showFooter = true,
  showLogo = true,
}:mainWindowProps):TemplateResult => html`
  <div class="l-container">
    ${showLogo ? html`<div class="c-logo">${icLogo}</div>` : ""}
    <div class="c-card c-card--background">
      <div class="c-card c-card--highlight">
        ${slot}
      </div>
      ${showFooter ? footer : ""}
    </div>
  </div>
`;