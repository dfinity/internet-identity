import { Purpose } from "$generated/internet_identity_types";
import { warningLabelIcon } from "$src/components/infoScreen";
import { mainWindow } from "$src/components/mainWindow";
import { DynamicKey, I18n } from "$src/i18n";
import { renderPage, withRef } from "$src/utils/lit-html";
import { html } from "lit-html";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";
import copyJson from "./confirmRemoveDevice.json";

// 'authentication' | 'recovery' from Purpose type
export type PurposeType = "authentication" | "recovery";
const confirmRemoveDeviceTemplate = ({
  i18n,
  purpose,
  next,
  cancel,
  alias,
}: {
  i18n: I18n;
  purpose: Purpose;
  next: () => void;
  cancel: () => void;
  alias: string;
}) => {
  const copy = i18n.i18n(copyJson);
  const mapper: Record<
    PurposeType,
    { title: DynamicKey; message: DynamicKey }
  > = {
    authentication: {
      title: copy.title_authenticator_device,
      message: copy.message_authenticator_device,
    },
    recovery: {
      title: copy.title_recovery_device,
      message: copy.message_recovery_device,
    },
  };
  const purposeType = Object.keys(purpose)[0] as PurposeType;
  const mappedCopy = mapper[purposeType];
  const input: Ref<HTMLInputElement> = createRef();
  const confirmButton: Ref<HTMLButtonElement> = createRef();

  const slot = html`
    <hgroup data-page="confirm-remove-device-page">
      <div class="c-card__label c-card__label--hasIcon">
        ${warningLabelIcon}
        <h2>${copy.label}</h2>
      </div>
      <h1 class="t-title t-title--main">${mappedCopy.title}</h1>
      <p class="t-paragraph">${mappedCopy.message}</p>
    </hgroup>
    <div class="l-stack">
      ${purposeType === "authentication"
        ? html`<input
            autofocus
            ${ref(input)}
            id="confirmRemoveDevice"
            class="c-input c-input--stack c-input--fullwidth"
            spellcheck="false"
            @change=${() =>
              withRef(input, (inputElement) =>
                withRef(confirmButton, (buttonElement) => {
                  if (inputElement.value === alias) {
                    buttonElement.disabled = false;
                  } else {
                    buttonElement.disabled = true;
                  }
                })
              )}
          />`
        : undefined}
    </div>
    <div class="l-stack">
      <button
        .disabled=${purposeType === "authentication"}
        ${ref(confirmButton)}
        @click=${() => next()}
        data-action="next"
        class="c-button"
      >
        ${copy.confirm}
      </button>
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--secondary"
      >
        ${copy.cancel}
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const confirmRemoveDevicePage = renderPage(confirmRemoveDeviceTemplate);

export const confirmRemoveDevice = ({
  i18n,
  purpose,
  alias,
}: {
  i18n: I18n;
  purpose: Purpose;
  alias: string;
}): Promise<"confirmed" | "cancelled"> => {
  return new Promise((resolve) =>
    confirmRemoveDevicePage({
      i18n,
      purpose,
      alias,
      next: () => resolve("confirmed"),
      cancel: () => resolve("cancelled"),
    })
  );
};
