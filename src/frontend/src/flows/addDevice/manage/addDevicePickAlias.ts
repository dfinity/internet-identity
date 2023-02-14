import { html, render } from "lit-html";
import { logoutSection } from "../../../components/logout";
import { validateAlias } from "../../../utils/validateAlias";
import { createRef, ref } from "lit-html/directives/ref.js";
import { withRef } from "../../../utils/lit-html";
// Regex Pattern for input: All characters, must be alphabet or number
// Can have hyphen(s), space(s) or underscore(s) in the middle.
// Good examples: "2019_macbook", "2019-Macbook", "2019 Macbook"
// Bad examples: "2019 macbook!", "2010 macbook_", "space trails at end "

const pickDeviceAliasTemplate = ({
  cancel,
  pick,
}: {
  cancel: () => void;
  pick: (alias: string) => void;
}) => {
  const deviceInput = createRef<HTMLInputElement>();
  const deviceAliasForm = createRef<HTMLFormElement>();
  return html`
    <article class="l-container c-card c-card--highlight">
      <hgroup>
        <h1 class="t-title t-title--main">Add a Trusted Device</h1>
        <p class="t-lead">What device are you using?</p>
      </hgroup>
      <form
        @submit=${(event: SubmitEvent) => {
          event.preventDefault();
          withRef(deviceInput, (deviceInput) => {
            pick(deviceInput.value);
          });
        }}
        ${ref(deviceAliasForm)}
        id="deviceAliasForm"
      >
        <input
          ${ref(deviceInput)}
          @invalid=${() => {
            withRef(deviceInput, (deviceInput) => {
              const message = validateAlias(
                deviceInput.validity,
                deviceInput.value
              );
              deviceInput.setCustomValidity(message);
            });
          }}
          @input=${() => {
            withRef(deviceInput, (deviceInput) => {
              deviceInput.setCustomValidity("");
              deviceInput.reportValidity();
            });
          }}
          class="c-input inputDeviceAlias"
          aria-label="device name"
          id="deviceAlias"
          placeholder="Example: my phone"
          type="text"
          required
          maxlength="30"
          pattern="^[A-Za-z0-9]+((-|\\s|_)*[A-Za-z0-9])*$"
          spellcheck="false"
        />
        <div class="l-stack">
          <button type="submit" id="deviceAliasContinue" class="c-button">
            Add Device
          </button>
          <button
            @click=${() => cancel()}
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
};

export const pickDeviceAliasPage = (
  props: Parameters<typeof pickDeviceAliasTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(pickDeviceAliasTemplate(props), contain);
};

/**
 * Shows a page prompting the user to pick a device alias.
 */
export const pickDeviceAlias = async (): Promise<string | null> => {
  return new Promise((resolve) => {
    pickDeviceAliasPage({
      cancel: () => resolve(null),
      pick: (alias) => resolve(alias),
    });
  });
};
