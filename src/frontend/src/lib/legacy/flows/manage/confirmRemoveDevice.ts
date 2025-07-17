import type { Purpose } from "$lib/generated/internet_identity_types";
import { warningLabelIcon } from "$lib/templates/infoScreen";
import { mainWindow } from "$lib/templates/mainWindow";
import { LEGACY_II_URL } from "$lib/config";
import { DynamicKey, I18n } from "$lib/legacy/i18n";
import { renderPage, withRef } from "$lib/utils/lit-html";
import { formatLastUsage } from "$lib/utils/time";
import { nonNullish } from "@dfinity/utils";
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
  lastUsedNanoseconds,
  originRegistered,
  isCurrentDevice,
}: {
  i18n: I18n;
  purpose: Purpose;
  next: () => void;
  cancel: () => void;
  alias?: string;
  lastUsedNanoseconds: bigint | undefined;
  originRegistered: string | undefined;
  isCurrentDevice: boolean;
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
  const input: Ref<HTMLInputElement> = createRef();
  const confirmButton: Ref<HTMLButtonElement> = createRef();
  const mappedCopy = mapper[purposeType];

  let lastUsageFormattedString: string | undefined = undefined;
  if (nonNullish(lastUsedNanoseconds)) {
    const lastUsageTimeStamp = new Date(
      Number(lastUsedNanoseconds / BigInt(1000000)),
    );
    lastUsageFormattedString = formatLastUsage(lastUsageTimeStamp);
  }

  const showAlias =
    purposeType === "authentication" && nonNullish(alias) && alias !== "";

  const slot = html`
    <hgroup data-page="confirm-remove-device-page">
      <div class="c-card__label c-card__label--hasIcon">
        ${warningLabelIcon}
        <h2 class="c-warning">${copy.label}</h2>
      </div>
      <h1 class="t-title t-title--main">${mappedCopy.title}</h1>
      <p class="t-paragraph">${copy.irreversible_action}</p>
      <ul class="c-list c-list--bulleted">
        ${
          showAlias
            ? html`<li>
                ${copy.label_nickname} <span class="t-strong">${alias}</span>
              </li>`
            : undefined
        }
        ${
          nonNullish(lastUsageFormattedString)
            ? html`<li>
                ${copy.label_last_used}
                <span class="t-strong">${lastUsageFormattedString}</span>
              </li>`
            : undefined
        }
        <li>${html`${copy.label_origin}
          <span class="t-strong"
            >${originRegistered ?? LEGACY_II_URL}</span
          >`}</strong></li>
      </ul>
      <p class="t-paragraph">${mappedCopy.message}</p>
      ${
        isCurrentDevice
          ? html`<p class="t-paragraph">${copy.current_device_warning}</p>`
          : undefined
      }
    </hgroup>
    <div>
      ${
        purposeType === "authentication"
          ? html`<input
              autofocus
              ${ref(input)}
              id="confirmRemoveDeviceAlias"
              class="c-input c-input--stack c-input--fullwidth"
              spellcheck="false"
              .onpaste=${(e: Event) => e.preventDefault()}
              @input=${() =>
                withRef(input, (inputElement) =>
                  withRef(confirmButton, (buttonElement) => {
                    if (inputElement.value === alias) {
                      buttonElement.disabled = false;
                    } else {
                      buttonElement.disabled = true;
                    }
                  }),
                )}
            />`
          : undefined
      }
    </div>
    <div class="l-stack">
      <button
        .disabled=${purposeType === "authentication"}
        ${ref(confirmButton)}
        @click=${() => next()}
        id="confirmRemoveDeviceButton"
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
  lastUsedNanoseconds,
  originRegistered,
  isCurrentDevice,
}: {
  i18n: I18n;
  purpose: Purpose;
  alias: string;
  lastUsedNanoseconds: bigint | undefined;
  originRegistered: string | undefined;
  isCurrentDevice: boolean;
}): Promise<"confirmed" | "cancelled"> => {
  return new Promise((resolve) =>
    confirmRemoveDevicePage({
      i18n,
      purpose,
      alias,
      next: () => resolve("confirmed"),
      cancel: () => resolve("cancelled"),
      lastUsedNanoseconds,
      originRegistered,
      isCurrentDevice,
    }),
  );
};
