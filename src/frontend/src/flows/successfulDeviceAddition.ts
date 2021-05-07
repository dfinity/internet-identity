import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../components/logout";
import { IDPActor } from "../utils/idp_actor";
import { renderManage } from "./manage";

const pageContent = (name: string) => html`
  <div class="container">
    <h1>Success!</h1>
    <p>You have successfully added your new device.</p>
    <label>Device name:</label>
    <div class="highlightBox">
      ${name}
    </div>
    <button id="manageDevicesButton" class="primary">Manage devices</button>
    ${logoutSection()}
  </div>
  `;

export const successfullyAddedDevice = async (name: string, userNumber: bigint, connection: IDPActor): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(name), container);
  initLogout();
  init(userNumber, connection);
}

const init = async (userNumber: bigint, connection: IDPActor) => {
  const manageDevicesButton = document.getElementById("manageDevicesButton") as HTMLButtonElement;
  manageDevicesButton.onclick = () => renderManage(userNumber, connection)
}
