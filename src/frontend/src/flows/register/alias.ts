import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { withRef } from "../../utils/lit-html";
import { html, render, TemplateResult } from "lit-html";
import { validateAlias } from "../addDevice/validateAlias";
import { mainWindow } from "../../components/mainWindow";

/* Everything (template, component, page) related to picking a device alias */

export const promptDeviceAliasTemplate = (props: {
  continue: (alias: string) => void;
  cancel: () => void;
}): TemplateResult => {
  const aliasInput: Ref<HTMLInputElement> = createRef();
  const promptDeviceAliasSlot = html`
    <hgroup class="t-centered">
      <h1 class="t-title t-title--main">Register this device</h1>
      <p class="t-lead t-paragraph l-stack">What device are you using?</p>
    </hgroup>
    <form
      id="registerForm"
      @submit=${(e: SubmitEvent) => {
        e.preventDefault();
        e.stopPropagation();
        withRef(aliasInput, (alias) => props.continue(alias.value));
      }}
    >
      <input
        id="registerAlias"
        ${ref(aliasInput)}
        @input=${(e: InputEvent) => {
          if (!(e.currentTarget instanceof HTMLInputElement)) {
            return;
          }
          e.currentTarget.setCustomValidity("");
          e.currentTarget.reportValidity();
        }}
        @invalid=${(e: InputEvent) => {
          if (!(e.currentTarget instanceof HTMLInputElement)) {
            return;
          }
          const message = validateAlias(
            e.currentTarget.validity,
            e.currentTarget.value
          );
          e.currentTarget.setCustomValidity(message);
        }}
        placeholder="Example: my phone"
        aria-label="device name"
        type="text"
        required
        maxlength="30"
        pattern="^[A-Za-z0-9]+((-|\\s|_)*[A-Za-z0-9])*$"
        spellcheck="false"
        class="c-input"
      />
      <div class="c-button-group">
        <button
          id="registerCancel"
          type="button"
          class="c-button c-button--secondary"
          @click="${() => props.cancel()}"
        >
          Cancel
        </button>
        <button id="registerButton" type="submit" class="c-button">Next</button>
      </div>
    </form>
  `;

  return mainWindow({
    showFooter: false,
    slot: promptDeviceAliasSlot,
  });
};

export const promptDeviceAliasPage = (props: {
  cancel: () => void;
  continue: (alias: string) => void;
  container?: HTMLElement;
}): void => {
  const container =
    props.container ?? (document.getElementById("pageContent") as HTMLElement);
  render(promptDeviceAliasTemplate(props), container);
};

export const promptDeviceAlias = (): Promise<string | null> =>
  new Promise((resolve) => {
    promptDeviceAliasPage({ cancel: () => resolve(null), continue: resolve });
  });
