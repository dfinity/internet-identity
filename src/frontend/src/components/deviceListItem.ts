import { TemplateResult, html } from "lit-html";
import { Setting } from "../flows/manage/deviceSettings";
import { warningIcon, dropdownIcon, lockIcon } from "./icons";

// A simple representation of "device"s used on the manage page.
export type Device = {
  // All the settings allowed for a particular device
  settings: Setting[];
  // The displayed name of a device (not exactly the "alias") because
  // recovery devices handle aliases differently.
  label: string;
  isRecovery: boolean;
  isProtected: boolean;
  warn?: TemplateResult;
};

// A device with extra information about whether another device (earlier in the list)
// has the same name.
export type DedupDevice = Device & { dupCount?: number };

export const deviceListItem = ({
  device,
  index,
}: {
  device: DedupDevice;
  index: string;
}) => {
  return html`
    <div class="c-action-list__label" data-device=${device.label}>
      ${device.label}
      ${device.dupCount !== undefined && device.dupCount > 0
        ? html`<i class="t-muted">&nbsp;(${device.dupCount})</i>`
        : undefined}
    </div>
    ${device.isProtected
      ? html`<div class="c-action-list__action">
          <span
            class="c-tooltip c-tooltip--left c-icon c-icon--lock"
            tabindex="0"
            >${lockIcon}<span class="c-tooltip__message c-card c-card--tight"
              >Your device is protected</span
            ></span
          >
        </div>`
      : undefined}
    ${device.warn !== undefined
      ? html`<div class="c-action-list__action">
          <span
            class="c-tooltip c-tooltip--left c-icon c-icon--warning"
            tabindex="0"
            >${warningIcon}<span class="c-tooltip__message c-card c-card--tight"
              >${device.warn}</span
            ></span
          >
        </div>`
      : undefined}
    ${device.settings.length > 0
      ? html` <div class="c-action-list__action c-dropdown">
          <button
            class="c-dropdown__trigger c-action-list__action"
            aria-expanded="false"
            aria-controls="dropdown-${index}"
            data-device=${device.label}
          >
            ${dropdownIcon}
          </button>
          <ul class="c-dropdown__menu" id="dropdown-${index}">
            ${device.settings.map((setting) => {
              return html` <li class="c-dropdown__item">
                <button
                  class="c-dropdown__link"
                  data-device=${device.label}
                  data-action=${setting.label}
                  @click=${() => setting.fn()}
                >
                  ${setting.label}
                </button>
              </li>`;
            })}
          </ul>
        </div>`
      : undefined}
  `;
};
