import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../components/logout";
import { IIConnection } from "../utils/iiConnection";
import { renderManage } from "./manage";

const pageContent = (name: string) => html`
  <div class="l-container c-card c-card--highlight">
    <h1 class="t-title t-title--main">Success!</h1>
    <p class="t-lead">You have successfully added your new device.</p>
    <div class="l-section">
      <h2 class="t-title">Device name:</h2>
      <data class="c-input c-input--readonly t-vip">${name}</data>
      <button id="manageDevicesButton" class="c-button">Manage devices</button>
    </div>
    ${logoutSection()}
  </div>
`;

export const successfullyAddedDevice = async (
  name: string,
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(name), container);
  initLogout();
  init(userNumber, connection);
};

const init = async (userNumber: bigint, connection: IIConnection) => {
  const manageDevicesButton = document.getElementById(
    "manageDevicesButton"
  ) as HTMLButtonElement;
  manageDevicesButton.onclick = () => renderManage(userNumber, connection);
};
