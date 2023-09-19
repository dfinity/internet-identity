import { mainWindow } from "$src/components/mainWindow";
import { PinResult, pinInput } from "$src/components/pinInput";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { isNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { pinStepper } from "./stepper";

import copyJson from "./setPin.json";

/* Prompt the user to create a new PIN */

const setPinTemplate = ({
  i18n,
  cancel,
  onContinue,
  focus = false,
  scrollToTop = false,
}: {
  i18n: I18n;
  cancel: () => void;
  onContinue: (pin: string) => void;
  /* put the page into view */
  scrollToTop?: boolean;
  focus?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const pinInput_ = pinInput({
    onSubmit: onContinue,
    verify: (pin) => verifyPin(pin),
    secret: true,
    focus,
  });
  const slot = html`
    ${pinStepper({ current: "set_pin" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.set_pin_for_ii}</h1>
    </hgroup>
    <div class="l-stack" data-role="set-pin">
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

// Ensure the PIN is wellformed
const verifyPin = (pin: string): PinResult<string> => {
  if (pin.length < 6) {
    return { ok: false, error: "PIN is too short, should be 6 digits" };
  }

  if (pin.length > 6) {
    return { ok: false, error: "PIN is too long, should be 6 digits" };
  }

  if (isNullish(pin.match(/^[0-9]+$/))) {
    return { ok: false, error: "PIN should only contain digits" };
  }

  return { ok: true, value: pin };
};

export const setPinPage = renderPage(setPinTemplate);
export const setPin = (): Promise<
  { tag: "ok"; pin: string } | { tag: "canceled" }
> => {
  return new Promise((resolve) =>
    setPinPage({
      i18n: new I18n(),
      onContinue: (pin: string) => resolve({ tag: "ok", pin }),
      cancel: () => resolve({ tag: "canceled" }),
      focus: true,
      scrollToTop: true,
    })
  );
};
