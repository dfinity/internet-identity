import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";
import { warningIcon } from "./icons";

// A device with extra information about whether another device (earlier in the list)
// has the same name.
export type DedupDevice = Device & { dupCount?: number };

// Deduplicate devices with same (duplicated) labels
const dedupLabels = (authenticators: Device[]): DedupDevice[] => {
  return authenticators.reduce<Device[]>((acc, authenticator) => {
    const _authenticator: DedupDevice = { ...authenticator };
    const sameName = acc.filter((a) => a.label === _authenticator.label);
    if (sameName.length >= 1) {
      _authenticator.dupCount = sameName.length + 1;
    }

    acc.push(_authenticator);
    return acc;
  }, []);
};

export const devicesSection = ({
  authenticators,
  onAddDevice,
  maxAuthenticators,
}: {
  authenticators: Device[];
  onAddDevice: () => void;
  maxAuthenticators: number;
}): TemplateResult => {
  const wrapClasses = ["l-stack"];
  const isWarning = authenticators.length < 2;

  if (isWarning === true) {
    wrapClasses.push("c-card", "c-card--narrow", "c-card--warning");
  }

  const _authenticators = dedupLabels(authenticators);

  return html`
    <aside class="${wrapClasses.join(" ")}">
      ${
        isWarning === true
          ? html`<span class="c-card__icon" aria-hidden="true"
              >${warningIcon}</span
            >`
          : undefined
      }
      <div class="${isWarning === true ? "c-card__content" : undefined}">
        <div class="t-title t-title--complications">
          <h2 class="t-title">Added devices</h2>
          <span class="t-title__complication c-tooltip" tabindex="0">
            <span class="c-tooltip__message c-card c-card--tight">
              You can register up to ${maxAuthenticators} authenticator
              devices (recovery devices excluded)</span>
              (${_authenticators.length}/${maxAuthenticators})
            </span>
          </span>
        </div>
        ${
          isWarning === true
            ? html`<p class="warning-message t-paragraph t-lead">
                We recommend that you have at least two devices (for example,
                your computer and your phone).
              </p>`
            : undefined
        }

        <div class="c-action-list">
          <div id="deviceList">
          <ul>
          ${_authenticators.map((device, index) => {
            return html`
              <li class="c-action-list__item">
                ${deviceListItem({
                  device,
                  index: `authenticator-${index}`,
                })}
              </li>
            `;
          })}</ul>
          </div>
          <div class="c-action-list__actions">
            <button
              ?disabled=${_authenticators.length >= maxAuthenticators}
              class="c-button c-button--primary c-tooltip c-tooltip--onDisabled c-tooltip--left"
              @click="${() => onAddDevice()}"
              id="addAdditionalDevice"
            >
              <span class="c-tooltip__message c-card c-card--tight"
                >You can register up to ${maxAuthenticators} authenticator devices.
                Remove a device before you can add a new one.</span
              >
              <span>Add new device</span>
            </button>
          </div>

        </div>
      </div>
    </aside>`;
};
