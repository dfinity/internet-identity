import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../components/logout";
import { clearHash } from "./addDevice";

const pageContent = () => html`
<div class="container">
    <h1>New Device</h1>
    <p>What alias would you like to give the new device?</p>
    <input id="deviceAlias" placeholder="Device alias">
    <button id="deviceAliasContinue" class="primary">Link Device</button>
    <button id="deviceAliasCancel">Cancel</button>
    ${logoutSection()}
</div>
  `;

export const pickDeviceAlias = async (): Promise<string> => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(), container);
    return init()
}

const init = (): Promise<string> => new Promise(resolve => {
    initLogout();
    const deviceAlias = document.getElementById("deviceAlias") as HTMLInputElement;
    const deviceAliasContinue = document.getElementById("deviceAliasContinue") as HTMLButtonElement;
    const deviceAliasCancel = document.getElementById("deviceAliasCancel") as HTMLButtonElement;
    deviceAliasCancel.onclick = () => {
        clearHash();
        window.location.reload();
    }
    deviceAliasContinue.onclick = () => {
        resolve(deviceAlias.value)
    }
});
