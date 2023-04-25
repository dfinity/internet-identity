import { PORTAL_II_URL } from "$root/config";
import { html, TemplateResult } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";

export const navigationLink = ({
  labelText,
  id,
  url,
  classes,
  rel,
}: {
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
  >${labelText}</a
>`;

export const footer = html`<footer class="l-footer">
  ${navigationLink({
    labelText: "Home",
    id: "homeLink",
    url: "/",
    classes: "t-link--discreet l-footer__link",
  })}
  ${navigationLink({
    labelText: "About",
    id: "aboutLink",
    url: PORTAL_II_URL,
    classes: "t-link--discreet l-footer__link",
  })}
  ${navigationLink({
    labelText: "FAQ",
    id: "faqLink",
    url: "https://support.dfinity.org/hc/en-us/sections/8730568843412-Internet-Identity",
    classes: "t-link--discreet l-footer__link",
    rel: "noopener noreferrer",
  })}
</footer>`;
