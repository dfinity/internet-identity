import { mainWindow } from "$lib/templates/mainWindow";
import { I18n } from "$lib/legacy/i18n";
import { renderPage, withRef } from "$lib/utils/lit-html";
import { TemplateResult, html } from "lit-html";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";

import copyJson from "./index.json";

/* Everything (template, component, page) related to picking a device alias */

export const promptDeviceAliasTemplate = (props: {
  title: string;
  message?: string | TemplateResult;
  cancelText?: string;
  value?: string;
  continue: (alias: string) => void;
  cancel: () => void;
  i18n: I18n;
}): TemplateResult => {
  const copy = props.i18n.i18n(copyJson);

  const aliasInput: Ref<HTMLInputElement> = createRef();
  const promptDeviceAliasSlot = html`
    <hgroup class="t-centered" data-page="prompt-device-alias">
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
        withRef(aliasInput, (alias) => props.continue(alias.value.trim()));
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
          const message = validationMessage(e.currentTarget.validity);
          e.currentTarget.setCustomValidity(message);
        }}
        placeholder=${copy.placeholder}
        value=${ifDefined(props.value)}
        aria-label="device name"
        type="text"
        required
        maxlength="30"
        pattern="^(\\s*\\S+\\s*)+$"
        spellcheck="false"
        class="c-input c-input--stack c-input--fullwidth"
      />
      <div class="c-button-group">
        <button
          id="pickAliasCancel"
          type="button"
          class="c-button c-button--secondary"
          @click="${() => props.cancel()}"
          data-action="skip"
        >
          ${props.cancelText ?? copy.cancel}
        </button>
        <button
          id="pickAliasSubmit"
          type="submit"
          class="c-button"
          data-action="next"
        >
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

/**
 * Returns a validation message based on the validity of the input.
 * The only constraint is that the name can't consist of only whitespace.
 * Good examples: "2019_macbook", " 2019-Macböök  " (note: will be trimmed on submit), "🚀"
 * Bad examples: "", "  "
 * @param valueMissing Whether the input is empty.
 * @param patternMismatch Whether the input consists of only whitespace.
 */
const validationMessage = ({
  valueMissing,
  patternMismatch,
}: {
  valueMissing: boolean;
  patternMismatch: boolean;
}): string => {
  if (valueMissing) {
    return "Name can't be empty.";
  } else if (patternMismatch) {
    return "Name can't consist of only whitespace.";
  }
  return "";
};

export const promptDeviceAliasPage = renderPage(promptDeviceAliasTemplate);

export const promptDeviceAlias = ({
  title,
  message,
  value,
  cancelText,
}: {
  title: string;
  message?: string | TemplateResult;
  value?: string;
  cancelText?: string;
}): Promise<string | null> =>
  new Promise((resolve) => {
    const i18n = new I18n();
    promptDeviceAliasPage({
      title,
      message,
      value,
      cancelText,
      cancel: () => resolve(null),
      continue: resolve,
      i18n,
    });
  });
