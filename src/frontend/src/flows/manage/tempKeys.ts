import { warningIcon } from "$src/components/icons";
import {
  authenticatorItem,
  dedupLabels,
} from "$src/flows/manage/authenticatorsSection";
import { Authenticator } from "$src/flows/manage/types";
import { I18n } from "$src/i18n";
import { unreachable } from "$src/utils/utils";
import { TemplateResult, html } from "lit-html";

import copyJson from "./tempKeys.json";

export type TempKeysWarning =
  | { tag: "add_recovery"; action: () => void }
  | { tag: "add_passkey"; action: () => void };
export const tempKeyWarningSection = ({
  i18n,
  tempKeysWarning,
}: {
  i18n: I18n;
  tempKeysWarning: TempKeysWarning;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const warningButtonCopy = (tempKeysWarning: TempKeysWarning) => {
    switch (tempKeysWarning.tag) {
      case "add_recovery":
        return copy.add_recovery_phrase;
      case "add_passkey":
        return copy.add_new_passkey;
      default:
        unreachable(tempKeysWarning, "unknown temp keys warning tag");
    }
  };

  return html`
    <aside class="c-card c-card--narrow c-card--warning">
      <span class="c-card__label c-card__label--hasIcon" aria-hidden="true">
        <i class="c-card__icon c-icon c-icon--error__flipped c-icon--inline"
          >${warningIcon}</i
        >
        <h2>${copy.security_warning}</h2>
      </span>
      <div class="t-title t-title--complications">
        <h2 class="t-title">${copy.you_are_using_temporary_key}</h2>
      </div>
      <p class="warning-message t-paragraph t-lead">
        ${copy.set_up_recovery_and_passkey}
      </p>
      <button
        class="c-button c-button--primary"
        @click="${tempKeysWarning.action}"
        id="addRecovery"
      >
        <span>${warningButtonCopy(tempKeysWarning)}</span>
      </button>
    </aside>
  `;
};

export const tempKeysSection = ({
  authenticators: authenticators_,
  i18n,
}: {
  authenticators: Authenticator[];
  i18n: I18n;
}): TemplateResult => {
  const authenticators = dedupLabels(authenticators_);
  const copy = i18n.i18n(copyJson);

  return html` <aside class="l-stack c-card c-card--narrow">
    <div class="t-title t-title--complications">
      <h2 class="t-title">${copy.temporary_key}</h2>
    </div>

    <p class="t-paragraph t-lead">${copy.key_stored_in_browser}</p>
    <div class="c-action-list">
      <ul>
        ${authenticators.map((authenticator, index) =>
          authenticatorItem({ authenticator, index })
        )}
      </ul>
    </div>
  </aside>`;
};
