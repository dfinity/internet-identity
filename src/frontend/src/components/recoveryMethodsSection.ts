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
                label: "Phrase",
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
                label: "Device",
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
                  <h2>
                    Recovery
                    methods${recoveries.length > 1
                      ? html`<i
                          class="c-icon c-icon--success c-icon--inline-after"
                          >${checkmarkIcon}</i
                        >`
                      : null}
                  </h2>
                </div>
                ${sectionList({ recoveries, onAddRecovery })}
              `
        }
      </details>
    </aside>
  `;
};
