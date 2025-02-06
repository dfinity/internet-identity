import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";
import copyJson from "./selectAuthMethod.json";

export const selectAuthMethodTemplate = ({
  onCreateWithPasskey,
  onCreateWithGoogle,
  onCancel,
}: {
  onCreateWithPasskey: () => void;
  onCreateWithGoogle: () => void;
  onCancel: () => void;
}): TemplateResult => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);

  const selectAuthMethodSlot = html`
    <hgroup data-page="prompt-auth-method">
      <header class="l-stack">
        <h1 class="t-title t-title--main">${copy.create_internet_identity}</h1>
        <p class="t-lead">${copy.to_continue}</p>
      </header>
    </hgroup>

    <div class="c-button-group--stack">
      <button class="c-button c-button--primary" @click=${onCreateWithPasskey}>
        ${copy.continue_with_passkey}
      </button>
      <button class="c-button c-button--secondary" @click=${onCreateWithGoogle}>
        ${copy.continue_with_google}
      </button>
      <button class="c-button c-button--textOnly" @click=${onCancel}>
        ${copy.cancel}
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    slot: selectAuthMethodSlot,
  });
};

export const selectAuthnMethodPage = renderPage(selectAuthMethodTemplate);

export const selectAuthMethod = (): Promise<
  "passkey" | "google" | "canceled"
> =>
  new Promise((resolve) => {
    selectAuthnMethodPage({
      onCreateWithGoogle: () => resolve("google"),
      onCreateWithPasskey: () => resolve("passkey"),
      onCancel: () => resolve("canceled"),
    });
  });
