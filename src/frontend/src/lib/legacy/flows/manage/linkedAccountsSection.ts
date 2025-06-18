import type { OpenIdCredential } from "$lib/generated/internet_identity_types";
import { googleIcon, pulsatingCircleIcon } from "$lib/templates/icons";
import copyJson from "$lib/legacy/flows/manage/linkedAccountsSection.json";
import { I18n } from "$lib/legacy/i18n";
import { getMetadataString } from "$lib/utils/openID";
import { formatLastUsage } from "$lib/utils/time";
import { nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { settingsDropdown } from "./settingsDropdown";

// The maximum number of linked accounts we allow.
// The canister limits the _total_ number of linked accounts to 100,
// and we (the frontend) won't show a counter since this number is intentionally
// high to avoid showing a usage counter while still having an actual limit.
const MAX_CREDENTIALS = 100;

export const linkedAccountsSection = ({
  credentials,
  onLinkAccount,
  onUnlinkAccount,
  hasOtherAuthMethods,
  currentCredential,
}: {
  credentials: OpenIdCredential[];
  onLinkAccount: () => void;
  onUnlinkAccount: (credential: OpenIdCredential) => void;
  hasOtherAuthMethods: boolean;
  currentCredential?: Pick<OpenIdCredential, "iss" | "sub">;
}): TemplateResult => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);
  const unlinkAvailable = credentials.length > 1 || hasOtherAuthMethods;

  return html`<aside
    class="l-stack c-card c-card--narrow"
    data-role="linked-accounts"
  >
    <h2 class="t-title">Linked Accounts</h2>
    <p class="t-paragraph t-lead">
      ${credentials.length === 0
        ? copy.link_your_account_to_hold_assets_and_sign_into_dapps
        : copy.use_your_accounts_to_hold_assets_and_sign_into_dapps}
    </p>
    <div class="c-action-list">
      <ul>
        ${credentials.map((credential, index) =>
          accountItem({
            credential,
            index,
            unlink: unlinkAvailable ? onUnlinkAccount : undefined,
            isCurrent:
              nonNullish(currentCredential) &&
              credential.iss === currentCredential.iss &&
              credential.sub === currentCredential.sub,
          }),
        )}
      </ul>
      <div class="c-action-list__actions">
        <button
          ?disabled=${credentials.length >= MAX_CREDENTIALS}
          class="c-button c-button--primary c-tooltip c-tooltip--onDisabled c-tooltip--left"
          @click="${() => onLinkAccount()}"
          id="linkAccount"
        >
          <span class="c-tooltip__message c-card c-card--tight">
            ${copy.max_linked_accounts_reached}
          </span>
          <span>
            ${credentials.length === 0
              ? copy.link_account
              : copy.link_additional_account}
          </span>
        </button>
      </div>
    </div>
  </aside>`;
};

export const accountItem = ({
  credential,
  index = 0,
  unlink,
  isCurrent,
}: {
  credential: OpenIdCredential;
  index?: number;
  unlink?: (credential: OpenIdCredential) => void;
  isCurrent?: boolean;
}) => {
  const i18n = new I18n();
  const copy = i18n.i18n(copyJson);
  const settings = [
    {
      action: "unlink",
      caption: copy.unlink.toString(),
      fn: () => unlink?.(credential),
    },
  ];

  const lastUsageTimeStamp = credential.last_usage_timestamp[0];
  const lastUsageFormattedString = nonNullish(lastUsageTimeStamp)
    ? formatLastUsage(new Date(Number(lastUsageTimeStamp / BigInt(1000000))))
    : undefined;
  const name = getMetadataString(credential.metadata, "name");
  const email = getMetadataString(credential.metadata, "email");
  const picture = getMetadataString(credential.metadata, "picture");

  return html`
    <li class="c-action-list__item" data-account=${credential.sub}>
      ${
        nonNullish(picture)
          ? html`<div class="c-action-list__avatar">
              <img src="${picture}" alt="" aria-hidden="true" loading="lazy" />
              <div class="c-action-list__avatar--badge">${googleIcon}</div>
            </div>`
          : ""
      }
      <div class="c-action-list__label--stacked c-action-list__label">
        <div class="c-action-list__label c-action-list__label--spacer">
          <div class="c-action-list__label">
            <span class="c-tooltip" tabindex="0">
              <span class="c-tooltip__message c-card c-card--tight t-nowrap">${email}</span
              <span>${name}</span>
            </span>
          </div>
          ${
            nonNullish(unlink)
              ? settingsDropdown({
                  alias: credential.sub,
                  id: `account-${index}`,
                  settings,
                })
              : ""
          }
        </div>
        <div>
          ${
            nonNullish(isCurrent) && isCurrent
              ? html`<div>
                  <span class="c-icon c-icon--ok c-icon--xs"
                    >${pulsatingCircleIcon}</span
                  >
                  <span class="t-muted">${copy.current_account_label}</span>
                </div>`
              : nonNullish(lastUsageFormattedString)
                ? html`<div class="t-muted">
                    ${copy.last_used}: ${lastUsageFormattedString}
                  </div>`
                : ""
          }
        </div>
      </div>
    </li>
  `;
};
