import { TemplateResult, html } from "lit-html";
import { deviceListItem, Device } from "./deviceListItem";
import { warningIcon } from "./icons";

// The maximum number of authenticator (non-recovery) devices we allow.
// The canister limits the _total_ number of devices (recovery included) to 10,
// and we (the frontend) only allow user one recovery device per type (phrase, fob),
// which leaves room for 8 authenticator devices.
const MAX_AUTHENTICATORS = 8;

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
}: {
  authenticators: Device[];
  onAddDevice: () => void;
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
              You can register up to ${MAX_AUTHENTICATORS} authenticator
              devices (recovery devices excluded)</span>
              (${_authenticators.length}/${MAX_AUTHENTICATORS})
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
          <ul>
          ${_authenticators.map((device, index) =>
            deviceListItem({ device, index: `authenticator-${index}` })
          )}</ul>
          <div class="c-action-list__actions">
            <button
              ?disabled=${_authenticators.length >= MAX_AUTHENTICATORS}
              class="c-button c-button--primary c-tooltip c-tooltip--onDisabled c-tooltip--left"
              @click="${() => onAddDevice()}"
              id="addAdditionalDevice"
            >
              <span class="c-tooltip__message c-card c-card--tight"
                >You can register up to ${MAX_AUTHENTICATORS} authenticator devices.
                Remove a device before you can add a new one.</span
              >
              <span>Add new device</span>
            </button>
          </div>

        </div>
      </div>
    </aside>`;
};
