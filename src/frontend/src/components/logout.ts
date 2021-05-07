import { html, TemplateResult } from "lit-html";
import { clearHash } from "../flows/addDevice";
import { logoutIcon } from "./icons";

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
