import { html } from "lit-html";
import { aboutLink } from "../components/aboutLink";
import { faqLink } from "../components/faqLink";

export const navbar = html`<div id="navbar">
  ${aboutLink} &middot; ${faqLink}
</div>`;
