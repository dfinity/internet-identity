import { html, render } from "lit";
import { logoutSection } from "../components/logout";
import { AuthenticatedConnection } from "../utils/iiConnection";
import { renderManage } from "./manage";

const pageContent = (name: string) => html`
  <div class="l-container c-card c-card--highlight">
    <h1 class="t-title t-title--main">Success!</h1>
    <p class="t-lead">You have successfully added your new device.</p>
    <div class="l-stack">
      <h2 class="t-title">Device name:</h2>
      <output class="c-input c-input--readonly t-vip">${name}</output>
      <button id="manageDevicesButton" class="c-button">Manage devices</button>
    </div>
    ${logoutSection()}
  </div>
`;

export const successfullyAddedDevice = async (
  connection: AuthenticatedConnection,
  name: string,
  userNumber: bigint
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(name), container);
  init(userNumber, connection);
};

const init = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
) => {
  const manageDevicesButton = document.getElementById(
    "manageDevicesButton"
  ) as HTMLButtonElement;
  manageDevicesButton.onclick = () => renderManage(userNumber, connection);
};
