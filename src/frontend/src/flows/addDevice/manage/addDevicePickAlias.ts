import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../../../components/logout";
import { startCardAnimation } from "../../../utils/animation";
import { validateAlias } from "../validateAlias";
// Regex Pattern for input: All characters, must be alphabet or number
// Can have hyphen(s), space(s) or underscore(s) in the middle.
// Good examples: "2019_macbook", "2019-Macbook", "2019 Macbook"
// Bad examples: "2019 macbook!", "2010 macbook_", "space trails at end "

// TODO: not in the showcase

const pageContent = () => html`
  <article class="l-container c-card c-card--bg">
    <div class="c-card-bg">
      <canvas class="c-card-bg__canvas" width="32" height="32"></canvas>
    </div>

    <hgroup>
      <h1 class="t-title t-title--main">New Device</h1>
      <p class="t-lead">Please provide a name for your new device</p>
    </hgroup>
    <form id="deviceAliasForm">
      <div class="c-animated-input">
        <input
          class="c-animated-input__button c-input inputDeviceAlias"
          aria-label="device name"
          id="deviceAlias"
          placeholder="Device alias"
          type="text"
          required
          maxlength="30"
          pattern="^[A-Za-z0-9]+((-|\\s|_)*[A-Za-z0-9])*$"
          spellcheck="false"
        />
        <button
          type="submit"
          id="deviceAliasContinue"
          class="c-animated-input__button c-button"
        >
          Add Device
        </button>
        <canvas class="c-animated-input__bg" width="32" height="32"></canvas>
      </div>

      <div class="l-section">
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
  startCardAnimation();
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
    const deviceInput = document.getElementById(
      "deviceAlias"
    ) as HTMLInputElement;

    deviceInput.addEventListener("invalid", () => {
      const message = validateAlias(deviceInput.validity, deviceInput.value);
      deviceInput.setCustomValidity(message);
    });
    deviceInput.addEventListener("input", () => {
      deviceInput.setCustomValidity("");
      deviceInput.reportValidity();
    });
  });
