import { html, TemplateResult } from "lit-html";

export const navigationLink = ({
  labelText,
  id,
  url,
  classes,
}: {
  labelText: string;
  id: string;
  url: string;
  classes: string;
}): TemplateResult => html`<a
  id="${id}"
  class="${classes}"
  href="${url}"
  target="_blank"
  >${labelText}</a
>`;

export const footer = html`<footer class="l-footer">
  ${navigationLink({
    labelText: "Home",
    id: "homeLink",
    url: "/",
    classes: "t-link--discreet l-footer__elem",
  })}
  ${navigationLink({
    labelText: "About",
    id: "aboutLink",
    url: "/about",
    classes: "t-link--discreet l-footer__elem",
  })}
  ${navigationLink({
    labelText: "FAQ",
    id: "faqLink",
    url: "https://support.dfinity.org/hc/en-us/sections/8730568843412-Internet-Identity",
    classes: "t-link--discreet l-footer__elem",
  })}
</footer>`;
