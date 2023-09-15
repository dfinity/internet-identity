import { mainWindow } from "$src/components/mainWindow";
import { PinResult, pinInput } from "$src/components/pinInput";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";
import { pinStepper } from "./stepper";

import copyJson from "./confirmPin.json";

/* PIN confirmation (just prompts the user to re-enter their PIN) */

const confirmPinTemplate = ({
  i18n,
  cancel,
  onContinue,
  expectedPin: expectedPin,
  focus = false,
  scrollToTop = false,
}: {
  i18n: I18n;
  cancel: () => void;
  onContinue: () => void;
  expectedPin: string;
  focus?: boolean;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const verify = (pin: string): PinResult<string> => {
    if (pin === expectedPin) {
      return { ok: true, value: pin };
    }

    // XXX: there is currently no way to restart the PIN. We need to figure
    // out what to do UX-wise first.
    return { ok: false, error: "PINs don't match" };
  };

  const pinInput_ = pinInput({
    onSubmit: (_pin) => onContinue(),
    verify,
    secret: true,
    focus,
  });
  const slot = html`
    ${pinStepper({ current: "set_pin" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.confirm_pin}</h1>
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
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const confirmPinPage = renderPage(confirmPinTemplate);
export const confirmPin = ({
  expectedPin,
}: {
  expectedPin: string;
}): Promise<{ tag: "ok" } | { tag: "canceled" }> => {
  return new Promise((resolve) =>
    confirmPinPage({
      i18n: new I18n(),
      onContinue: () => resolve({ tag: "ok" }),
      expectedPin,
      cancel: () => resolve({ tag: "canceled" }),
      focus: true,
      scrollToTop: true,
    })
  );
};
