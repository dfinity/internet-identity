import { html, TemplateResult } from "lit-html";
import { dropdownIcon } from "../../components/icons";

export const settingsDropdown = ({
  alias,
  id,
  settings,
}: {
  alias: string;
  id: string;
  settings: { fn: () => void; caption: string; action: string }[];
}): TemplateResult => {
  return html` <div class="c-action-list__action c-dropdown">
    <button
      class="c-dropdown__trigger c-action-list__action"
      aria-expanded="false"
      aria-controls="dropdown-${id}"
      aria-label="Open settings"
      data-device=${alias}
    >
      ${dropdownIcon}
    </button>
    <ul class="c-dropdown__menu" id="dropdown-${id}">
      ${settings.map(
        (setting) => html`
          <li class="c-dropdown__item">
            <button
              class="c-dropdown__link"
              data-device=${alias}
              data-action=${setting.action}
              @click=${() => setting.fn()}
            >
              ${setting.caption}
            </button>
          </li>
        `
      )}
    </ul>
  </div>`;
};
