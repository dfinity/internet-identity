import { html, TemplateResult } from "lit-html";
import { logoutIcon } from "./icons";

// Add a button at the bottom of the page. Clicking the button clears the local
// storage and redirects to the welcome page.

export const logoutSection = (
  alternativeLabel?: string
): TemplateResult => html`<style>
    #logoutIcon {
      position: relative;
      top: 6px;
    }
    #logoutBox {
      margin-top: 3rem;
    }
  </style>
  <div id="logoutBox">
    <hr />
    <button type="button" class="linkStyle" id="logoutButton">
      ${logoutIcon}
      ${alternativeLabel !== undefined ? alternativeLabel : "Logout"}
    </button>
  </div>`;

export const initLogout = (): void => {
  const logoutButton = document.getElementById("logoutButton") as HTMLElement;
  logoutButton.onclick = () => {
    localStorage.clear();
    clearHash();
    window.location.reload();
  };
};

const clearHash = (): void => {
  history.pushState(
    // Preserve the #authorize hash if it's present.
    /authorize/.test(window.location.hash) ? "authorize" : "",
    document.title,
    window.location.pathname + window.location.search
  );
};
