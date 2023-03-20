import { TemplateResult, html } from "lit-html";
import { Setting, settingName } from "../flows/manage/deviceSettings";
import {
  warningIcon,
  errorIcon,
  successIcon,
  dropdownIcon,
  lockIcon,
} from "./icons";

// A simple representation of "device"s used on the manage page.
export type Device = {
  // All the settings allowed for a particular device
  settings?: Setting[];
  // The displayed name of a device (not exactly the "alias") because
  // recovery devices handle aliases differently.
  label: string | TemplateResult;
  recovery?: "phrase" | "device";
  isProtected?: boolean;
  status?: {
    // the status types are used to determine the icon & color to display
    // and because of that used in the CSS
    statusType: "error" | "warning" | "ok";
    statusText?: TemplateResult;
  };
};

const iconByStatusType = {
  error: errorIcon,
  warning: warningIcon,
  ok: successIcon,
};

// A device with extra information about whether another device (earlier in the list)
// has the same name.
export type DedupDevice = Device & { dupCount?: number };

export const deviceListItem = ({
  device,
  index,
  block,
}: {
  device: DedupDevice;
  index: string;
  block?: TemplateResult;
}) => {
  return html`
    <li class="c-action-list__item" data-device=${device.label}>
      ${device.status !== undefined
        ? html`<div class="c-action-list__status">
            <span
              class="c-tooltip c-icon c-icon--${device.status.statusType}"
              tabindex="0"
              >${iconByStatusType[device.status.statusType]}
              ${device.status.statusText !== undefined
                ? html`<span class="c-tooltip__message c-card c-card--tight"
                    >${device.status.statusText}</span
                  >`
                : null}
            </span>
          </div>`
        : undefined}
      <div class="c-action-list__label">
        ${device.label}
        ${device.dupCount !== undefined && device.dupCount > 0
          ? html`<i class="t-muted">&nbsp;(${device.dupCount})</i>`
          : undefined}
      </div>
      ${device.isProtected !== undefined && device.isProtected
        ? html`<div class="c-action-list__action" data-role="protected">
            <span class="c-tooltip c-icon c-icon--lock" tabindex="0"
              >${lockIcon}<span class="c-tooltip__message c-card c-card--tight"
                >Your device is locked</span
              ></span
            >
          </div>`
        : undefined}
      ${block !== undefined ? block : undefined}
      ${device.settings && device.settings.length > 0
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
                    ${settingName[setting.label]}
                  </button>
                </li>`;
              })}
            </ul>
          </div>`
        : undefined}
    </li>
  `;
};
