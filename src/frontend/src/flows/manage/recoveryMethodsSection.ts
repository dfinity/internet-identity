import { html, TemplateResult } from "lit-html";
import {
  checkmarkRoundIcon,
  lockedIcon,
  warningIcon,
  warningRoundIcon,
} from "../../components/icons";
import {
  recoveryKeyLabel,
  recoveryPhraseLabel,
} from "../../utils/recoveryDevice";
import { settingsDropdown } from "./settingsDropdown";
import { Devices, RecoveryKey, RecoveryPhrase } from "./types";

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
        <h2>Recovery methods</h2>
      </div>
      <div class="c-action-list">
        <ul>
          ${recoveryPhrase === undefined
            ? missingRecovery({ recovery: "phrase", addRecoveryPhrase })
            : recoveryPhraseItem({ recoveryPhrase })}
          ${recoveryKey === undefined
            ? missingRecovery({ recovery: "key", addRecoveryKey })
            : recoveryKeyItem({ recoveryKey })}
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
}: {
  recoveryPhrase: RecoveryPhrase;
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
  return html`
    <li class="c-action-list__item" data-device=${alias}>
      ${checkmark()}
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
}: {
  recoveryKey: RecoveryKey;
}) => {
  const alias = recoveryKeyLabel;
  const id = "recovery-key";
  const settings = [
    { action: "remove", caption: "Remove", fn: () => recoveryKey.remove() },
  ];
  return html`
    <li class="c-action-list__item" data-device=${alias}>
      ${checkmark()}
      <div class="c-action-list__label">${alias}</div>
      ${settingsDropdown({ alias, id, settings })}
    </li>
  `;
};

const checkmark = (): TemplateResult => {
  return html`
    <div class="c-action-list__status">
      <span class="c-icon c-icon--ok c-tooltip"
        >${checkmarkRoundIcon}
        <span class="c-tooltip__message c-card c-card--tight"
          >You enabled a recovery phrase.</span
        >
      </span>
    </div>
  `;
};
