import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { I18n } from "$src/i18n";
import { IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { renderPage, TemplateElement } from "$src/utils/lit-html";
import { unknownToString } from "$src/utils/utils";
import { constructIdentity } from "$src/utils/webAuthn";
import { isCancel, webAuthnErrorCopy } from "$src/utils/webAuthnErrorUtils";
import { html, TemplateResult } from "lit-html";

import copyJson from "./passkey.json";

/* Anchor construction component (for creating WebAuthn credentials) */

const savePasskeyTemplate = ({
  construct,
  i18n,
}: {
  construct: () => void;
  i18n: I18n;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const slot = html`
    <hgroup>
      <h1 class="t-title t-title--main">${copy.save_passkey}</h1>
      <p class="t-paragraph">
        ${copy.select} <strong class="t-strong">${copy.save_passkey}</strong> ${
    copy.and_complete_prompts
  }</p>
    </hgroup>
    <button @click=${() =>
      construct()} data-action="construct-identity" class="c-button">${
    copy.save_passkey
  }</button>
    <p class="t-paragraph">
    <strong class="t-strong">${copy.what_is_passkey}</strong>
    <ul class="c-list c-list--bulleted">
    <li>${copy.convenient_secure_replacement}</li>
    <li>${copy.enables_sign_by_unlocking}</li>
    </ul>
    </p>

    <p class="t-paragraph">
    <strong class="t-strong">${copy.what_happens_save_passkey}</strong>
    <ul class="c-list c-list--bulleted">
    <li>${copy.your_device_prompt_authenticate}</li>
    <li>${copy.no_personal_data}</li>
    <li>${copy.no_software_downloaded}</li>
    </ul>
    </p>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const savePasskeyPage = renderPage(savePasskeyTemplate);

// Prompt the user to create a WebAuthn identity
export const savePasskey = (): Promise<IIWebAuthnIdentity> => {
  return new Promise((resolve) =>
    savePasskeyPage({
      i18n: new I18n(),
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
      <strong class="t-strong">${copy.cancel_title}</strong
      >:${copy.cancel_message}
    </p>`;
  } else {
    return html`<p class="t-paragraph">
      <strong class="t-strong">Failed to create identity</strong>: An error
      occured during construction: ${unknownToString(e, "unknown error")}
    </p>`;
  }
};
