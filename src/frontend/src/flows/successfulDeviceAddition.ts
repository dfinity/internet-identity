import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../components/logout";
import { startCardAnimation } from "../utils/animation";
import { AuthenticatedConnection } from "../utils/iiConnection";
import { renderManage } from "./manage";

// TODO: not presented in the showcase

const pageContent = (name: string) => html`
  <div class="l-container c-card c-card--bg">
    <div class="c-card-bg">
      <canvas class="c-card-bg__canvas" width="32" height="32"></canvas>
    </div>
    <h1 class="t-title t-title--main">Success!</h1>
    <p class="t-lead">You have successfully added your new device.</p>
    <div class="l-section">
      <h2 class="t-title">Device name:</h2>

      <div class="c-animated-input">
        <output class="c-animated-input__input c-input c-input--readonly t-vip"
          >${name}</output
        >
        <button
          id="manageDevicesButton"
          class="c-animated-input__button c-animated-input__button--vip c-button"
        >
          Manage devices
        </button>
        <canvas class="c-animated-input__bg" width="32" height="32"></canvas>
      </div>
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
  startCardAnimation();
  initLogout();
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
