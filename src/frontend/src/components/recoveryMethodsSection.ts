import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";
import { warnBox } from "./warnBox";

const recoveryNag = ({ onAddRecovery }: { onAddRecovery: () => void }) =>
  warnBox({
    title: "Recovery method",
    message: "Add a recovery method to help protect this Identity Anchor.",
    additionalClasses: ["l-stack"],
    slot: html`<button
      @click="${onAddRecovery}"
      id="addRecovery"
      class="c-button"
    >
      Add Recovery
    </button>`,
  });

// The list of recovery devices
export const recoveryMethodsSection = ({
  recoveries,
  onAddRecovery,
}: {
  recoveries: Device[];
  onAddRecovery: () => void;
}): TemplateResult => {
  if (recoveries.length === 0) {
    return recoveryNag({ onAddRecovery });
  }
  return html`
    <aside class="l-stack">
      <details open>
        ${recoveries.length === 0
          ? undefined
          : html`
              <summary class="t-title">
                <h2>Recovery methods</h2>
              </summary>
              <div class="c-action-list">
                <ul>
                  ${recoveries.map((device, index) =>
                    deviceListItem({
                      device,
                      index: `recovery-${index}`,
                    })
                  )}
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
      </details>
    </aside>
  `;
};
