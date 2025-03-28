import { OpenIdCredential } from "$generated/internet_identity_types";
import { warningLabelIcon } from "$src/components/infoScreen";
import { mainWindow } from "$src/components/mainWindow";
import { accountItem } from "$src/flows/manage/linkedAccountsSection";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { nonNullish } from "@dfinity/utils";
import { html } from "lit-html";
import copyJson from "./confirmUnlinkAccount.json";

const confirmUnlinkAccountTemplate = ({
  i18n,
  credential,
  next,
  cancel,
  isCurrentCredential,
}: {
  i18n: I18n;
  credential: OpenIdCredential;
  next: () => void;
  cancel: () => void;
  isCurrentCredential?: boolean;
}) => {
  const copy = i18n.i18n(copyJson);

  const slot = html`
    <hgroup data-page="confirm-unlink-account-page">
      <div class="c-card__label c-card__label--hasIcon">
        ${warningLabelIcon}
        <h2>${copy.label}</h2>
      </div>
      <h1 class="t-title t-title--main">${copy.title}</h1>
      <p class="t-paragraph">${copy.message}</p>
      ${accountItem({ credential })}
      ${nonNullish(isCurrentCredential) && isCurrentCredential
        ? html`<p class="t-paragraph">${copy.current_credential_warning}</p>`
        : undefined}
    </hgroup>
    <div class="l-stack">
      <button
        @click=${() => next()}
        id="confirmUnlinkAccountButton"
        data-action="next"
        class="c-button"
      >
        ${copy.confirm}
      </button>
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--secondary"
      >
        ${copy.cancel}
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const confirmUnlinkAccountPage = renderPage(
  confirmUnlinkAccountTemplate
);

export const confirmUnlinkAccount = ({
  i18n,
  credential,
  isCurrentCredential,
}: {
  i18n: I18n;
  credential: OpenIdCredential;
  isCurrentCredential?: boolean;
}): Promise<"confirmed" | "cancelled"> => {
  return new Promise((resolve) =>
    confirmUnlinkAccountPage({
      i18n,
      credential,
      next: () => resolve("confirmed"),
      cancel: () => resolve("cancelled"),
      isCurrentCredential,
    })
  );
};
