import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";
import { warnBox } from "./warnBox";

const recoveryByType = ({
  type,
  recoveries,
}: {
  type: "phrase" | "device";
  recoveries: Device[];
}) => recoveries.filter((device) => device.recovery === type);

const recoveryNag = ({
  recoveries,
  onAddRecovery,
}: {
  recoveries: Device[];
  onAddRecovery: () => void;
}) =>
  warnBox({
    title: "Recovery method",
    message: "Add a recovery method to help protect this Identity Anchor.",
    additionalClasses: ["l-stack"],
    slot: html`${sectionList({ recoveries })}`,
  });

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
                label: "Recovery phrase",
                recovery: "phrase",
              },
              index: "recovery-phrase",
              block: html`<button
                @click="${onAddRecovery}"
                class="c-button c-button--primary c-button--minimal"
                id="addRecoveryPhrase"
                aria-label="Add recovery phrase"
              >
                Add
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
                label: "Recovery device",
                recovery: "device",
              },
              index: "recovery-device",
              block: html`<button
                @click="${onAddRecovery}"
                class="c-button c-button--primary c-button--minimal"
                id="addRecoveryDevice"
                aria-label="Add recovery device"
              >
                Add
              </button>`,
            })}
      </ul>
      ${onAddRecovery === undefined
        ? undefined
        : html`<div class="c-action-list__actions">
            <button
              @click="${onAddRecovery}"
              class="c-button c-button--primary"
              id="addRecovery"
            >
              Add recovery method
            </button>
          </div>`}
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
  if (recoveries.length === 0) {
    return recoveryNag({ recoveries, onAddRecovery });
  }
  return html`
    <aside class="l-stack">
        ${
          recoveries.length === 0
            ? undefined
            : html`
                <div class="t-title">
                  <h2>Recovery methods</h2>
                </div>
                ${sectionList({ recoveries, onAddRecovery })}
              `
        }
      </details>
    </aside>
  `;
};
