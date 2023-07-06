import {
  checkmarkRoundIcon,
  lockedIcon,
  warningIcon,
  warningRoundIcon,
} from "$src/components/icons";
import { DynamicKey, I18n } from "$src/i18n";
import {
  recoveryKeyLabel,
  recoveryPhraseLabel,
} from "$src/utils/recoveryDevice";
import { isNullish } from "@dfinity/utils";
import { html, TemplateResult } from "lit-html";
import { settingsDropdown } from "./settingsDropdown";
import { Devices, RecoveryKey, RecoveryPhrase } from "./types";

import copyJson from "./recoveryMethodsSection.json";

// The list of recovery devices
export const recoveryMethodsSection = ({
  recoveries: { recoveryPhrase, recoveryKey },
  addRecoveryPhrase,
  addRecoveryKey,
}: {
  recoveries: Devices["recoveries"];
  addRecoveryPhrase: () => void;
  addRecoveryKey: () => void;
}): TemplateResult => {
  return html`
    <aside class="l-stack">
      <div class="t-title">
        <h2>Recovery Methods</h2>
      </div>
      <div class="c-action-list">
        <ul>
          ${isNullish(recoveryPhrase)
            ? missingRecovery({ recovery: "phrase", addRecoveryPhrase })
            : recoveryPhraseItem({ recoveryPhrase, i18n: new I18n() })}
          ${isNullish(recoveryKey)
            ? missingRecovery({ recovery: "key", addRecoveryKey })
            : recoveryKeyItem({ recoveryKey, i18n: new I18n() })}
        </ul>
      </div>
    </aside>
  `;
};

// Show a missing recovery method
export const missingRecovery = (
  args:
    | {
        recovery: "phrase";
        addRecoveryPhrase: () => void;
      }
    | {
        recovery: "key";
        addRecoveryKey: () => void;
      }
) => {
  const { icon, iconClass, recoveryName, message, fn, action } =
    args.recovery === "phrase"
      ? {
          icon: warningRoundIcon,
          iconClass: "c-icon--error",
          recoveryName: "Recovery Phrase",
          message: "For minimum security, enable a recovery phrase.",
          fn: () => args.addRecoveryPhrase(),
          action: "add-recovery-phrase",
        }
      : {
          icon: warningIcon,
          iconClass: "c-icon--warning",
          recoveryName: "Recovery Device",
          message: "For extra security, enable a recovery device.",
          fn: () => args.addRecoveryKey(),
          action: "add-recovery-device",
        };
  return html`
    <li class="c-action-list__item">
      <div class="c-action-list__status">
        <span class="c-tooltip c-icon ${iconClass}" tabindex="0"
          >${icon}
          <span class="c-tooltip__message c-card c-card--tight"
            >${message}</span
          >
        </span>
      </div>
      <div class="c-action-list__label">${recoveryName}</div>
      <button
        @click="${() => fn()}"
        class="c-button c-button--primary c-button--minimal"
        data-action=${action}
        aria-label="Add recovery phrase"
      >
        Enable
      </button>
    </li>
  `;
};

// List a recovery phrase
export const recoveryPhraseItem = ({
  recoveryPhrase,
  i18n,
}: {
  recoveryPhrase: RecoveryPhrase;
  i18n: I18n;
}) => {
  const alias = recoveryPhraseLabel;
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

  const { recovery_phrase_enabled } = i18n.i18n(copyJson);

  return html`
    <li class="c-action-list__item" data-device=${alias}>
      ${checkmark(recovery_phrase_enabled)}
      <div class="c-action-list__label">${alias}</div>
      ${recoveryPhrase.isProtected ? lock() : undefined}
      ${settingsDropdown({ alias, id, settings })}
    </li>
  `;
};

const lock = (): TemplateResult => {
  return html`
    <div class="c-action-list__status" data-role="protected">
      <span class="c-icon c-icon--ok c-icon--lock" tabindex="0"
        >${lockedIcon}</span
      >
    </div>
  `;
};

// List a recovery key (non-phrase recovery device)
export const recoveryKeyItem = ({
  recoveryKey,
  i18n,
}: {
  recoveryKey: RecoveryKey;
  i18n: I18n;
}) => {
  const alias = recoveryKeyLabel;
  const id = "recovery-key";
  const settings = [
    { action: "remove", caption: "Remove", fn: () => recoveryKey.remove() },
  ];

  const { recovery_key_enabled } = i18n.i18n(copyJson);

  return html`
    <li class="c-action-list__item" data-device=${alias}>
      ${checkmark(recovery_key_enabled)}
      <div class="c-action-list__label">${alias}</div>
      ${settingsDropdown({ alias, id, settings })}
    </li>
  `;
};

const checkmark = (label: DynamicKey | string): TemplateResult => {
  return html`
    <div class="c-action-list__status">
      <span class="c-icon c-icon--ok c-tooltip"
        >${checkmarkRoundIcon}
        <span class="c-tooltip__message c-card c-card--tight">${label}</span>
      </span>
    </div>
  `;
};
