import { html, TemplateResult } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";
import { renderPage, withRef } from "../../utils/lit-html";
import { validateAlias } from "../../utils/validateAlias";

import copyJson from "./index.json";

/* Everything (template, component, page) related to picking a device alias */

export const promptDeviceAliasTemplate = (props: {
  title: string;
  message?: string | TemplateResult;
  cancelText?: string;
  continue: (alias: string) => void;
  cancel: () => void;
  i18n: I18n;
}): TemplateResult => {
  const copy = props.i18n.i18n(copyJson);

  // static copy required because lit doesn't support template attributes (placeholder)
  // https://github.com/lit/lit/issues/1862
  const staticCopy = props.i18n.staticLang(copyJson);

  const aliasInput: Ref<HTMLInputElement> = createRef();
  const promptDeviceAliasSlot = html`
    <hgroup class="t-centered">
      <h1 class="t-title t-title--main">${props.title}</h1>
      <p class="t-lead t-paragraph l-stack">
        ${props.message ?? copy.specify_alias}
      </p>
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
        id="pickAliasInput"
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
        placeholder=${staticCopy.placeholder}
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
          id="pickAliasCancel"
          type="button"
          class="c-button c-button--secondary"
          @click="${() => props.cancel()}"
        >
          ${props.cancelText ?? copy.cancel}
        </button>
        <button id="pickAliasSubmit" type="submit" class="c-button">
          ${copy.next}
        </button>
      </div>
    </form>
  `;

  return mainWindow({
    showFooter: false,
    slot: promptDeviceAliasSlot,
  });
};

export const promptDeviceAliasPage = renderPage(promptDeviceAliasTemplate);

export const promptDeviceAlias = ({
  title,
  message,
  cancelText,
}: {
  title: string;
  message?: string | TemplateResult;
  cancelText?: string;
}): Promise<string | null> =>
  new Promise((resolve) => {
    const i18n = new I18n();
    promptDeviceAliasPage({
      title,
      message,
      cancelText,
      cancel: () => resolve(null),
      continue: resolve,
      i18n,
    });
  });
