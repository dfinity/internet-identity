import { mainWindow } from "$src/components/mainWindow";
import { pinInput } from "$src/components/pinInput";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";

import copyJson from "./usePin.json";

/* Prompt the user to input their PIN for use */

const usePinTemplate = ({
  i18n,
  cancel,
  onContinue,
  onUsePasskey,
  scrollToTop = false,
}: {
  i18n: I18n;
  cancel: () => void;
  onContinue: (pin: string) => void;
  onUsePasskey: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const pinInput_ = pinInput({
    onSubmit: onContinue,
    verify: (pin) =>
      // XXX: here we don't do any verification, but ideally the identity reconstruction should happen here
      ({ ok: true, value: pin }),
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
export const usePin = (): Promise<
  { kind: "pin"; pin: string } | { kind: "canceled" } | { kind: "passkey" }
> => {
  return new Promise((resolve) =>
    usePinPage({
      i18n: new I18n(),
      onUsePasskey: () => resolve({ kind: "passkey" }),
      onContinue: (pin: string) => resolve({ kind: "pin", pin }),
      cancel: () => resolve({ kind: "canceled" }),
      scrollToTop: true,
    })
  );
};
