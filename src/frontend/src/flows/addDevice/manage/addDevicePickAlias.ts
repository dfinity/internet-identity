import { html, render } from "lit-html";
import { initLogout, logoutSection } from "../../../components/logout";

const pageContent = () => html`
  <div class="container">
    <h1>New Device</h1>
    <form id="deviceAliasForm">
      <p>Please provide a name for your new device</p>
      <input
        class="inputDeviceAlias"
        aria-label="device name"
        id="deviceAlias"
        placeholder="Device alias"
        type="text"
        required
        maxlength="30"
        pattern="^[A-Za-z0-9_]+((-|\\s)*[A-Za-z0-9_])*$"
      />
      <button type="submit" id="deviceAliasContinue" class="primary">
        Add Device
      </button>
      <button type="button" id="deviceAliasCancel">Cancel</button>
    </form>
    ${logoutSection()}
  </div>
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
    const deviceInput = document.getElementById(
      "deviceAlias"
    ) as HTMLInputElement;

    deviceInput.addEventListener("invalid", () => {
      const firstChar = deviceInput.value[0];
      const lastChar = deviceInput.value.slice(-1);

      if (firstChar === " ") {
        deviceInput.setCustomValidity("Name can't start with a space.");
      } else if (lastChar === "-" || lastChar === " ") {
        deviceInput.setCustomValidity(
          "Name can't end with a hyphen or a space."
        );
      } else {
        deviceInput.setCustomValidity("Name can't contain special characters.");
      }
    });
    deviceInput.addEventListener("input", () => {
      deviceInput.setCustomValidity("");
      deviceInput.checkValidity();
    });
  });
