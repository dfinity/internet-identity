import { html, TemplateResult } from "lit-html";

export const navigationLink = (
  props = {
    labelText: "About",
    uuid: "aboutLink",
    url: "/about",
  }
): TemplateResult => html`<a
  id="${props.uuid}"
  class="c-nav-links__link t-link t-link--discreet"
  href="${props.url}"
  target="_blank"
  >${props.labelText}</a
>`;
