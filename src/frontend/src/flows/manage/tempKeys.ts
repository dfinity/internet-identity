import { cypherIcon } from "$src/components/icons";
import {
  authenticatorItem,
  dedupLabels,
} from "$src/flows/manage/authenticatorsSection";
import { Authenticator } from "$src/flows/manage/types";
import { I18n } from "$src/i18n";
import { TemplateResult, html } from "lit-html";

import copyJson from "./tempKeys.json";

export const tempKeysSection = ({
  authenticators: authenticators_,
  i18n,
}: {
  authenticators: Authenticator[];
  i18n: I18n;
}): TemplateResult => {
  const authenticators = dedupLabels(authenticators_);
  const copy = i18n.i18n(copyJson);

  return html` <aside
    class="l-stack c-card c-card--narrow"
    data-role="temp-keys"
  >
    <div class="t-title t-title--complications">
      <h2 class="t-title">${copy.temporary_key}</h2>
    </div>

    <p class="t-paragraph t-lead">${copy.key_stored_in_browser}</p>
    <div class="c-action-list">
      <ul>
        ${authenticators.map((authenticator, index) =>
          authenticatorItem({
            authenticator,
            index,
            icon: html`<span class="c-icon c-icon--pin"
              >${cypherIcon}<span></span
            ></span>`,
          })
        )}
      </ul>
    </div>
  </aside>`;
};
