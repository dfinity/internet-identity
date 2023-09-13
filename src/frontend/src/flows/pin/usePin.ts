import { mainWindow } from "$src/components/mainWindow";
import { pinInput, PinResult } from "$src/components/pinInput";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import type { TemplateResult } from "lit-html";
import { html } from "lit-html";

import copyJson from "./usePin.json";

/* Prompt the user to input their PIN for use */

const usePinTemplate = <T>({
  i18n,
  cancel,
  verify,
  onContinue,
  onUsePasskey,
  scrollToTop = false,
}: {
  i18n: I18n;
  cancel: () => void;
  verify: (pin: string) => Promise<PinResult<T>>;
  onContinue: (result: T) => void;
  onUsePasskey: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const pinInput_ = pinInput({
    onSubmit: onContinue,
    verify,
    secret: true,
  });
  const slot = html`
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.enter_pin_for_ii}</h1>
    </hgroup>
    <div class="l-stack">
      <div class="c-input--stack">${pinInput_.template}</div>
    </div>
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button c-button--secondary l-stack"
    >
      ${copy.cancel}
    </button>
    <button @click=${() => onUsePasskey()} class="c-button c-button--textOnly">
      ${copy.or_use_passkey}
    </button>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const usePinPage = renderPage(usePinTemplate);
export const usePin = <T>({
  verifyPin,
}: {
  verifyPin: (pin: string) => Promise<PinResult<T>>;
}): Promise<
  { kind: "pin"; result: T } | { kind: "canceled" } | { kind: "passkey" }
> => {
  return new Promise((resolve) =>
    renderPage(usePinTemplate<T>)({
      i18n: new I18n(),
      onUsePasskey: () => resolve({ kind: "passkey" }),
      verify: verifyPin,
      onContinue: (result: T) => resolve({ kind: "pin", result }),
      cancel: () => resolve({ kind: "canceled" }),
      scrollToTop: true,
    })
  );
};
