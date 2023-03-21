import { TemplateResult, html } from "lit-html";
import { lockIcon } from "../../components/icons";
import { RecoveryPhrase, RecoveryKey, Devices } from "./types";
import { settingsDropdown } from "./settingsDropdown";

// The list of recovery devices
export const recoveryMethodsSection = ({
  recoveries: { recoveryPhrase, recoveryKey },
  onAddRecovery,
}: {
  recoveries: Devices["recoveries"];
  onAddRecovery: () => void;
}): TemplateResult => {
  return html`
    <aside class="l-stack">
      ${recoveryPhrase === undefined && recoveryKey === undefined
        ? undefined
        : html`
            <div class="t-title">
              <h2>Recovery methods</h2>
            </div>
            <div class="c-action-list">
              <ul>
                ${recoveryPhrase === undefined
                  ? undefined
                  : recoveryPhraseItem({ recoveryPhrase })}
              </ul>
              <div class="c-action-list__actions">
                <button
                  @click="${onAddRecovery}"
                  class="c-button c-button--primary"
                  id="addRecovery"
                >
                  Add recovery method
                </button>
              </div>
            </div>
          `}
    </aside>
  `;
};

export const recoveryPhraseItem = ({
  recoveryPhrase,
}: {
  recoveryPhrase: RecoveryPhrase;
}) => {
  const alias = "Recovery Phrase";
  const id = "recovery-phrase";
  const settings = [
    { action: "reset", caption: "Reset", fn: () => recoveryPhrase.reset() },
    recoveryPhrase.isProtected
      ? {
          action: "unprotect",
          caption: "Unlock",
          fn: () => recoveryPhrase.unprotect(),
        }
      : {
          action: "protect",
          caption: "Lock",
          fn: () => recoveryPhrase.protect(),
        },
  ];
  return html`
    <li class="c-action-list__item" data-device=${alias}>
      <div class="c-action-list__label">${alias}</div>
      ${recoveryPhrase.isProtected
        ? html`<div class="c-action-list__action" data-role="protected">
            <span
              class="c-tooltip c-tooltip--left c-icon c-icon--lock"
              tabindex="0"
              >${lockIcon}<span class="c-tooltip__message c-card c-card--tight"
                >Your device is locked</span
              ></span
            >
          </div>`
        : undefined}
      ${settingsDropdown({ alias, id, settings })}
    </li>
  `;
};

export const recoveryKeyItem = ({
  recoveryKey,
}: {
  recoveryKey: RecoveryKey;
}) => {
  const alias = "Recovery Key";
  const id = "recovery-key";
  const settings = [
    { action: "remove", caption: "Remove", fn: () => recoveryKey.remove() },
  ];
  return html`
    <li class="c-action-list__item" data-device=${alias}>
      <div class="c-action-list__label">${alias}</div>
      ${settingsDropdown({ alias, id, settings })}
    </li>
  `;
};
