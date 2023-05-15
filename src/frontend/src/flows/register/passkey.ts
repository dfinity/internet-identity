import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { I18n } from "$src/i18n";
import { IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { mount, renderPage, TemplateElement } from "$src/utils/lit-html";
import { unknownToString } from "$src/utils/utils";
import { constructIdentity } from "$src/utils/webAuthn";
import { isCancel, webAuthnErrorCopy } from "$src/utils/webAuthnErrorUtils";
import { html, TemplateResult } from "lit-html";

import copyJson from "./passkey.json";

/* Anchor construction component (for creating WebAuthn credentials) */

const savePasskeyTemplate = ({
  construct,
  i18n,
  cancel,
  scrollToTop = false,
}: {
  construct: () => void;
  i18n: I18n;
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const slot = html`
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.save_passkey}</h1>
      <p class="t-paragraph">
        ${copy.select}
        <strong class="t-strong">${copy.save_passkey}</strong>
        ${copy.and_complete_prompts}
      </p>
    </hgroup>
    <button
      @click=${() => construct()}
      data-action="construct-identity"
      class="c-button"
    >
      ${copy.save_passkey}
    </button>
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button c-button--secondary"
    >
      ${copy.cancel}
    </button>
    <section style="margin-top: 7em;" class="c-marketing-block">
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

// Prompt the user to create a WebAuthn identity
export const savePasskey = (): Promise<IIWebAuthnIdentity | "canceled"> => {
  return new Promise((resolve) =>
    savePasskeyPage({
      i18n: new I18n(),
      cancel: () => resolve("canceled"),
      scrollToTop: true,
      construct: async () => {
        try {
          const identity = await withLoader(() => constructIdentity({}));
          resolve(identity);
        } catch (e) {
          toast.error(errorMessage(e));
        }
      },
    })
  );
};

// Return an appropriate error message depending on the (inferred) type of WebAuthn error
const errorMessage = (e: unknown): TemplateElement => {
  if (isCancel(e)) {
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
