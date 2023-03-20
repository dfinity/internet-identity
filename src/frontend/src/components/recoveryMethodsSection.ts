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
  return html`
    <aside class="l-stack">
      <h2 class="t-title">Recovery methods</h2>
      ${recoveries.length === 0
        ? // when you don't have any recovery methods, we want to nag the user to add one
          html`<p class="t-lead">
            We recommend that you create at least 1 recovery method.
          </p>`
        : null}
      ${sectionList({ recoveries, onAddRecovery })}
    </aside>
  `;
};
