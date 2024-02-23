import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { I18n } from "$src/i18n";
import { IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { mount, renderPage, TemplateElement } from "$src/utils/lit-html";
import { unknownToString } from "$src/utils/utils";
import { constructIdentity } from "$src/utils/webAuthn";
import {
  isWebAuthnCancel,
  webAuthnErrorCopy,
} from "$src/utils/webAuthnErrorUtils";
import { nonNullish } from "@dfinity/utils";
import { html, TemplateResult } from "lit-html";
import { registerStepper } from "./stepper";

import copyJson from "./passkey.json";

/* Anchor construction component (for creating WebAuthn credentials) */

const savePasskeyTemplate = ({
  constructPasskey,
  constructPin,
  i18n,
  cancel,
  scrollToTop = false,
}: {
  constructPasskey: () => void;
  constructPin?: () => void;
  i18n: I18n;
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const createPinButton = (constructPin: () => void) => html`
    <button
      @click=${() => constructPin()}
      data-action="construct-pin-identity"
      class="c-button c-button--secondary"
    >
      ${copy.without_passkey}
    </button>
  `;

  const slot = html`
    ${registerStepper({ current: "create" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.save_passkey}</h1>
      <p class="t-paragraph">
        ${copy.select}
        <strong class="t-strong">${copy.save_passkey}</strong>
        ${copy.and_complete_prompts}
      </p>
    </hgroup>
    <button
      @click=${() => constructPasskey()}
      data-action="construct-identity"
      class="c-button l-stack"
    >
      ${copy.save_passkey}
    </button>
    ${nonNullish(constructPin) ? createPinButton(constructPin) : ""}
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button c-button--textOnly"
    >
      ${copy.cancel}
    </button>
    <section class="c-marketing-block">
      <aside class="l-stack">
        <h3 class="t-title">${copy.what_is_passkey}</h3>
        <ul class="c-list c-list--bulleted">
          <li>${copy.unique_key_pair}</li>
          <li>${copy.convenient_secure_replacement}</li>
          <li>${copy.enables_sign_by_unlocking}</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">${copy.what_happens_save_passkey}</h3>
        <ul class="c-list c-list--bulleted">
          <li>${copy.your_device_prompt_authenticate}</li>
          <li>${copy.no_personal_data}</li>
          <li>${copy.no_software_downloaded}</li>
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

export const savePasskeyPage = renderPage(savePasskeyTemplate);

// Prompt the user to create a WebAuthn identity or a PIN identity (if allowed)
export const savePasskeyOrPin = ({
  pinAllowed,
}: {
  pinAllowed: boolean;
}): Promise<IIWebAuthnIdentity | "pin" | "canceled"> => {
  return new Promise((resolve) =>
    savePasskeyPage({
      i18n: new I18n(),
      cancel: () => resolve("canceled"),
      scrollToTop: true,
      constructPasskey: async () => {
        try {
          const identity = await withLoader(() => constructIdentity({}));
          resolve(identity);
        } catch (e) {
          toast.error(errorMessage(e));
        }
      },
      constructPin: pinAllowed ? () => resolve("pin") : undefined,
    })
  );
};

// Return an appropriate error message depending on the (inferred) type of WebAuthn error
const errorMessage = (e: unknown): TemplateElement => {
  if (isWebAuthnCancel(e)) {
    const copy = webAuthnErrorCopy();
    return html`<p class="t-paragraph">
      <strong class="t-strong">${copy.cancel_title}</strong>:
      ${copy.cancel_message}
    </p>`;
  } else {
    return html`<p class="t-paragraph">
      <strong class="t-strong">Failed to create identity</strong>: An error
      occured during construction: ${unknownToString(e, "unknown error")}
    </p>`;
  }
};
