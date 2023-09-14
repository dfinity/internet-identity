import { mainWindow } from "$src/components/mainWindow";
import { pinStepper } from "$src/flows/pin/stepper";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";

import copyJson from "./pinInfo.json";

const pinInfoTemplate = ({
  i18n,
  onContinue,
  cancel,
  scrollToTop = false,
}: {
  onContinue: () => void;
  i18n: I18n;
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const slot = html`
    ${pinStepper({ current: "set" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">
        ${copy.create_temporary_key_form_pin}
      </h1>
      <p class="t-paragraph">${copy.set_memorable_pin}</p>
    </hgroup>
    <button
      @click=${() => onContinue()}
      data-action="continue-pin"
      class="c-button"
    >
      ${copy.continue_and_set_pin}
    </button>
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button c-button--textOnly"
    >
      ${copy.cancel}
    </button>
    <section class="c-marketing-block">
      <aside class="l-stack">
        <h3 class="t-title">${copy.what_is_temporary_key}</h3>
        <ul class="c-list c-list--bulleted">
          <li>${copy.unique_key_pair}</li>
          <li>${copy.convenient_secure_replacement}</li>
          <li>${copy.enables_sign_by_pin}</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">${copy.why_temporary}</h3>
        <ul class="c-list c-list--bulleted">
          <li>${copy.clear_browser_storage}</li>
          <li>${copy.set_up_recovery}</li>
          <li>${copy.do_not_store_assets}</li>
        </ul>
      </aside>
    </section>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const pinInfoPage = renderPage(pinInfoTemplate);

// Show information about the PIN authentication (aka temporary key) and prompt
// the user to continue (or cancel).
export const promptPinInfo = (): Promise<"continue" | "canceled"> => {
  return new Promise((resolve) =>
    pinInfoPage({
      i18n: new I18n(),
      onContinue: () => resolve("continue"),
      cancel: () => resolve("canceled"),
      scrollToTop: true,
    })
  );
};
