import { html, TemplateResult } from "lit-html";
import { dscvrIcon, distriktIcon, openChatIcon } from "./icons";

export const linkTeaser = ({
  additionalClasses = [],
}: {
  additionalClasses?: string[];
}): TemplateResult => html`<article
  class="c-card c-card--narrow${additionalClasses.length > 0
    ? " " + additionalClasses.join(" ")
    : ""}"
>
  <h3 class="t-title t-title--sub">Explore dapps</h3>
  <p class="t-lead">
    Use your Identity Anchor to create independent accounts with dapps on the
    Internet Computer.
  </p>
  <ul class="c-list c-list--logos l-stack">
    <li>
      <a href="https://dscvr.one/" target="_blank" rel="noopener noreferrer">
        ${dscvrIcon}
        <span class="l-stack--tight t-strong">DSCVR</span>
      </a>
    </li>
    <li>
      <a href="https://distrikt.app/" target="_blank" rel="noopener noreferrer">
        ${distriktIcon}
        <span class="l-stack--tight t-strong">Distrikt</span>
      </a>
    </li>
    <li>
      <a href="https://oc.app/" target="_blank" rel="noopener noreferrer">
        ${openChatIcon}
        <span class="l-stack--tight t-strong">OpenChat</span>
      </a>
    </li>
  </ul>
</article>`;
