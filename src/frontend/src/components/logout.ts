import { nonNullish } from "@dfinity/utils";
import { html, TemplateResult } from "lit-html";
import { logoutIcon } from "./icons";

// Add a button at the bottom of the page. Clicking the button clears the local
// storage and redirects to the welcome page.

export const logoutSection = (
  alternativeLabel?: string
): TemplateResult => html`<div id="logoutBox" class="l-stack l-stack--spacious">
  <button type="button" class="t-link" @click="${logout}" id="logoutButton">
    <i class="t-link__icon">${logoutIcon}</i>
    ${nonNullish(alternativeLabel) ? alternativeLabel : "Logout"}
  </button>
</div>`;

const logout = () => {
  clearHash();
  window.location.reload();
};

const clearHash = (): void => {
  history.pushState(
    // Preserve the #authorize hash if it's present.
    /authorize/.test(window.location.hash) ? "authorize" : "",
    document.title,
    window.location.pathname + window.location.search
  );
};
