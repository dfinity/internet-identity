import { warnBox } from "$src/components/warnBox";
import { DynamicKey, I18n } from "$src/i18n";
import { unreachable } from "$src/utils/utils";
import { TemplateResult, html } from "lit-html";

import { isNullish } from "@dfinity/utils";
import copyJson from "./tempKeysWarning.json";

export type TempKeysWarning =
  | { tag: "add_recovery"; action: () => void }
  | { tag: "add_passkey"; action: () => void };
export const tempKeyWarning = ({
  i18n,
  tempKeysWarning,
}: {
  i18n: I18n;
  tempKeysWarning?: TempKeysWarning;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const warningButton = (tempKeysWarning?: TempKeysWarning) => {
    if (isNullish(tempKeysWarning)) {
      return undefined;
    }
    switch (tempKeysWarning.tag) {
      case "add_recovery":
        return button(tempKeysWarning.action, copy.add_recovery_phrase);
      case "add_passkey":
        return button(tempKeysWarning.action, copy.add_new_passkey);
      default:
        unreachable(tempKeysWarning, "unknown temp keys warning tag");
    }
  };

  return warnBox({
    title: copy.you_are_using_temporary_key,
    headerSlot: html`<h2>${copy.security_warning}</h2>`,
    message: copy.set_up_recovery_and_passkey,
    slot: warningButton(tempKeysWarning),
  });
};

const button = (action: () => void, buttonText: DynamicKey) => html`<button
  class="c-button c-button--primary l-stack"
  @click="${action}"
  id="addRecovery"
>
  <span>${buttonText}</span>
</button>`;
