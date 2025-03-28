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

import { analytics } from "$src/utils/analytics";
import copyJson from "./passkey.json";

/* Anchor construction component (for creating WebAuthn credentials) */

const savePasskeyTemplate = ({
  constructPasskey,
  constructPin,
  constructOpenIdGoogle,
  i18n,
  cancel,
  scrollToTop = false,
}: {
  constructPasskey: () => void;
  constructPin?: () => void;
  constructOpenIdGoogle?: () => void;
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
      ${copy.with_pin}
    </button>
  `;

  const createOpenIdButton = (constructOpenIdGoogle: () => void) => html`
    <button
      @click=${() => constructOpenIdGoogle()}
      data-action="construct-openid-identity"
      class="c-button c-button--secondary"
    >
      ${copy.openid_google}
    </button>
  `;

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
      @click=${() => constructPasskey()}
      data-action="construct-identity"
      class="c-button l-stack"
    >
      ${copy.save_passkey}
    </button>
    ${nonNullish(constructOpenIdGoogle)
      ? createOpenIdButton(constructOpenIdGoogle)
      : ""}
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
export const savePasskeyPinOrOpenID = async ({
  pinAllowed,
  origin,
  googleAllowed,
}: {
  pinAllowed: boolean;
  origin: string;
  googleAllowed: boolean;
}): Promise<IIWebAuthnIdentity | "pin" | "canceled" | "google" | undefined> => {
  if (pinAllowed || googleAllowed) {
    return new Promise((resolve) => {
      return savePasskeyPage({
        i18n: new I18n(),
        cancel: () => resolve("canceled"),
        scrollToTop: true,
        constructPasskey: async () => {
          analytics.event("construct-passkey");
          try {
            const rpId =
              origin === window.location.origin
                ? undefined
                : new URL(origin).hostname;
            const identity = await withLoader(() =>
              constructIdentity({ rpId })
            );
            analytics.event("construct-passkey-success");
            resolve(identity);
          } catch (e) {
            analytics.event("construct-passkey-error");
            toast.error(errorMessage(e));
          }
        },
        constructPin: pinAllowed ? () => resolve("pin") : undefined,
        constructOpenIdGoogle: googleAllowed
          ? () => resolve("google")
          : undefined,
      });
    });
  }
  try {
    const identity = await withLoader(() => constructIdentity({}));
    return identity;
  } catch (e) {
    toast.error(errorMessage(e));
    return undefined;
  }
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
