import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";

const recoveryByType = ({
  type,
  recoveries,
}: {
  type: "phrase" | "device";
  recoveries: Device[];
}) => recoveries.filter((device) => device.recovery === type);

const sectionList = ({
  recoveries,
  onAddRecovery,
}: {
  recoveries: Device[];
  onAddRecovery?: () => void;
}): TemplateResult => {
  const recoveryPhrases = recoveryByType({ type: "phrase", recoveries });
  const recoveryDevices = recoveryByType({ type: "device", recoveries });
  return html`
    <div class="c-action-list">
      <ul>
        ${recoveryPhrases.length > 0
          ? recoveryPhrases.map((device, index) =>
              deviceListItem({
                device,
                index: `recovery-phrase-${index}`,
              })
            )
          : deviceListItem({
              device: {
                label: "Recovery Phrase",
                recovery: "phrase",
                status: {
                  statusType: "error",
                  statusText: html`We strongly recommend that you use a recovery
                  phrase`,
                },
              },
              index: "recovery-phrase",
              block: html`<button
                @click="${onAddRecovery}"
                class="c-button c-button--primary c-button--minimal"
                id="addRecoveryPhrase"
                aria-label="Add recovery phrase"
              >
                Enable
              </button>`,
            })}
        ${recoveryDevices.length > 0
          ? recoveryDevices.map((device, index) =>
              deviceListItem({
                device,
                index: `recovery-device-${index}`,
              })
            )
          : deviceListItem({
              device: {
                label: "Recovery Device",
                recovery: "device",
                status: {
                  statusType: "warning",
                  statusText: html`we suggest that you use a recovery device`,
                },
              },
              index: "recovery-device",
              block: html`<button
                @click="${onAddRecovery}"
                class="c-button c-button--primary c-button--minimal"
                id="addRecoveryDevice"
                aria-label="Add recovery device"
              >
                Enable
              </button>`,
            })}
      </ul>
    </div>
  `;
};

// The list of recovery devices
export const recoveryMethodsSection = ({
  recoveries,
  onAddRecovery,
}: {
  recoveries: Device[];
  onAddRecovery: () => void;
}): TemplateResult => {
  /**
   * - when you don't have any recovery methods, we want to nag the user to add one
   * - when you have a single recovery method, we want to show it expanded
   * - when you have a recovery key and a recovery phrase, we want to show them both collapsed
   * - to keep the function simple we just check if there are less than 2 recovery methods
   */
  return html`
    <aside class="l-stack">
      <details class="c-details" .open="${recoveries.length < 2}">
        <summary class="t-title c-summary">
          <h2>Recovery methods</h2>
          <span class="c-summary__link c-summary__link--end"></span>
        </summary>

        ${recoveries.length === 0
          ? html`<p class="t-lead">
              We recommend that you create at least 1 recovery method.
            </p>`
          : null}
        ${sectionList({ recoveries, onAddRecovery })}
      </details>
    </aside>
  `;
};
