import { cypherIcon } from "$lib/templates/icons";
import {
  authenticatorItem,
  dedupLabels,
} from "$lib/legacy/flows/manage/authenticatorsSection";
import { Authenticator } from "$lib/legacy/flows/manage/types";
import { I18n } from "$lib/legacy/i18n";
import { unreachable } from "$lib/utils/utils";
import { TemplateResult, html } from "lit-html";

import type { DeviceWithUsage } from "$lib/generated/internet_identity_types";
import { warnBox } from "$lib/templates/warnBox";
import { nonNullish } from "@dfinity/utils";
import copyJson from "./tempKeys.json";

export type TempKeyWarningAction =
  | { tag: "add_recovery"; action: () => void }
  | { tag: "add_passkey"; action: () => void };
export const tempKeyWarningBox = ({
  i18n,
  warningAction,
}: {
  i18n: I18n;
  warningAction?: TempKeyWarningAction;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const warningButtonCopy = (tempKeysWarning: TempKeyWarningAction) => {
    switch (tempKeysWarning.tag) {
      case "add_recovery":
        return copy.add_recovery_phrase;
      case "add_passkey":
        return copy.add_new_passkey;
      default:
        unreachable(tempKeysWarning, "unknown temp keys warning tag");
    }
  };

  const buttonTemplate = nonNullish(warningAction)
    ? html`<button
        class="c-button c-button--primary l-stack"
        @click="${warningAction.action}"
        id="addRecovery"
      >
        <span>${warningButtonCopy(warningAction)}</span>
      </button>`
    : undefined;

  return warnBox({
    headerSlot: html`<h2>${copy.security_warning}</h2>`,
    title: copy.you_are_using_temporary_key,
    message: copy.set_up_recovery_and_passkey,
    slot: buttonTemplate,
  });
};

export const tempKeysSection = ({
  authenticators: authenticators_,
  i18n,
  onRemoveDevice,
}: {
  authenticators: Authenticator[];
  i18n: I18n;
  onRemoveDevice: (device: DeviceWithUsage) => void;
}): TemplateResult => {
  const authenticators = dedupLabels(authenticators_);
  const copy = i18n.i18n(copyJson);

  return html`<aside
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
            i18n,
            onRemove: () => onRemoveDevice(authenticator.device),
            icon: html`<span class="c-icon c-icon--pin"
              >${cypherIcon}<span></span
            ></span>`,
          }),
        )}
      </ul>
    </div>
  </aside>`;
};
