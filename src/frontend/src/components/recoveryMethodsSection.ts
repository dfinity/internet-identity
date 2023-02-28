import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";

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
      ${recoveries.length === 0
        ? undefined
        : html`
            <div class="t-title">
              <h2>Recovery methods</h2>
            </div>
            <div class="c-action-list">
              <div id="recoveryList">
                <ul>
                  ${recoveries.map(
                    (device, index) =>
                      html`
                        <li class="c-action-list__item">
                          ${deviceListItem({
                            device,
                            index: `recovery-${index}`,
                          })}
                        </li>
                      `
                  )}
                </ul>
              </div>
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
