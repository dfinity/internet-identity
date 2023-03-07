import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";
import { warnBox } from "./warnBox";
import { checkmarkIcon } from "./icons";

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
  console.log(recoveries);
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
