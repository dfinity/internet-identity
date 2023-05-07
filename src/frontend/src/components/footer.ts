import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { githubIcon, questionIcon } from "./icons";

export const navigationLink = ({
  icon,
  labelText,
  id,
  url,
  classes,
  rel,
}: {
  icon: TemplateResult;
  labelText: string;
  id: string;
  url: string;
  classes: string;
  rel?: string;
}): TemplateResult => html`<a
  id="${id}"
  class="${classes}"
  href="${url}"
  target="_blank"
  rel=${ifDefined(rel)}
  >${icon} ${labelText}</a
>`;

export const footer = html`<footer class="l-footer c-footer">
  <div class="c-footer--inner">
    ${navigationLink({
      icon: questionIcon,
      labelText: "Support",
      id: "support-link",
      url: "https://support.dfinity.org/hc/en-us/sections/8730568843412-Internet-Identity",
      rel: "noopener noreferrer",
      classes: "t-link--discreet c-footer__link",
    })}
    ${navigationLink({
      icon: githubIcon,
      labelText: "Source code",
      id: "source-link",
      url: "https://github.com/dfinity/internet-identity",
      rel: "noopener noreferrer",
      classes: "t-link--discreet c-footer__link",
    })}
  </div>
</footer>`;
