import { html } from "lit-html";
import { aboutLink } from "../components/aboutLink";
import { faqLink } from "../components/faqLink";

export const navbar = html`
<aside aria-label="General links">
  <ul class="nav-links">
    <li>${aboutLink}</li>
    <li>${faqLink}</li>
  </ul>
</aside>`;
