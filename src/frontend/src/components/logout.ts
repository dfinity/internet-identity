import { html } from "lit-html";
import { clearHash } from "../flows/addDevice";
import { logoutIcon } from "./icons/logout";

export const logoutSection = () => html`<style>
    #logoutIcon {
        position: relative;
        top: 6px;
    }
    #logoutBox {
        margin-top: 3rem;
    }
  </style>
  <div id="logoutBox">
    <hr>
    <button type="button" class="linkStyle" id="logoutButton">
      ${logoutIcon} Logout
    </button>
  </div>`;

export const initLogout = () => {
    const logoutButton = document.getElementById("logoutButton") as HTMLElement;
    logoutButton.onclick = () => {
        localStorage.clear();
        clearHash();
        window.location.reload()
    }
}
