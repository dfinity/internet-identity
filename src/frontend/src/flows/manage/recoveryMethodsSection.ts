import { TemplateResult, html } from "lit-html";
import { dropdownIcon, lockIcon } from "../../components/icons";
import { RecoveryPhrase, RecoveryKey, Devices } from "./types";

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
  const index = "recovery-phrase";
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
      <div class="c-action-list__action c-dropdown">
        <button
          class="c-dropdown__trigger c-action-list__action"
          aria-expanded="false"
          aria-controls="dropdown-${index}"
          data-device=${alias}
        >
          ${dropdownIcon}
        </button>
        <ul class="c-dropdown__menu" id="dropdown-${index}">
          <li class="c-dropdown__item">
            <button
              class="c-dropdown__link"
              data-device=${alias}
              data-action="reset"
              @click=${() => recoveryPhrase.reset()}
            >
              Reset
            </button>
          </li>

          ${recoveryPhrase.isProtected
            ? html`
                <li class="c-dropdown__item">
                  <button
                    class="c-dropdown__link"
                    data-device=${alias}
                    data-action="unprotect"
                    @click=${() => recoveryPhrase.unprotect()}
                  >
                    Unlock
                  </button>
                </li>
              `
            : html`
                <li class="c-dropdown__item">
                  <button
                    class="c-dropdown__link"
                    data-device=${alias}
                    data-action="protect"
                    @click=${() => recoveryPhrase.protect()}
                  >
                    Lock
                  </button>
                </li>
              `}
        </ul>
      </div>
    </li>
  `;
};

export const recoveryKeyItem = ({
  recoveryKey,
}: {
  recoveryKey: RecoveryKey;
}) => {
  const alias = "Recovery Key";
  const index = "recovery-key";
  return html`
    <li class="c-action-list__item" data-device=${alias}>
      <div class="c-action-list__label">${alias}</div>
      <div class="c-action-list__action c-dropdown">
        <button
          class="c-dropdown__trigger c-action-list__action"
          aria-expanded="false"
          aria-controls="dropdown-${index}"
          data-device=${alias}
        >
          ${dropdownIcon}
        </button>
        <ul class="c-dropdown__menu" id="dropdown-${index}">
          <li class="c-dropdown__item">
            <button
              class="c-dropdown__link"
              data-device=${alias}
              data-action="remove"
              @click=${() => recoveryKey.remove()}
            >
              Remove
            </button>
          </li>
        </ul>
      </div>
    </li>
  `;
};
