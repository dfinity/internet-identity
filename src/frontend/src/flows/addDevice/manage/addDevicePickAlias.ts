import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../../../components/logout";

const pageContent = () => html`
  <article class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">New Device</h1>
      <p class="t-lead">Please provide a name for your new device</p>
    </hgroup>
    <form id="deviceAliasForm">
      <input
        class="c-input inputDeviceAlias"
        aria-label="device name"
        id="deviceAlias"
        placeholder="Device alias"
        type="text"
        required
      />
      <div class="l-section">
        <button type="submit" id="deviceAliasContinue" class="c-button">
          Add Device
        </button>
        <button
          type="button"
          id="deviceAliasCancel"
          class="c-button c-button--secondary"
        >
          Cancel
        </button>
      </div>
    </form>
    ${logoutSection()}
  </article>
`;

/**
 * Shows a page prompting the user to pick a device alias.
 */
export const pickDeviceAlias = async (): Promise<string | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init();
};

const init = (): Promise<string | null> =>
  new Promise((resolve) => {
    initLogout();
    const deviceAlias = document.getElementById(
      "deviceAlias"
    ) as HTMLInputElement;
    const deviceAliasForm = document.getElementById(
      "deviceAliasForm"
    ) as HTMLButtonElement;
    const deviceAliasCancel = document.getElementById(
      "deviceAliasCancel"
    ) as HTMLButtonElement;
    deviceAliasCancel.onclick = () => {
      resolve(null);
    };
    deviceAliasForm.onsubmit = (event) => {
      event.preventDefault();
      resolve(deviceAlias.value);
    };
  });
