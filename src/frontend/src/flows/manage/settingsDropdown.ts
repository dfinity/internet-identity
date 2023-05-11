import { dropdownIcon } from "$src/components/icons";
import { handleKeyPress } from "$src/utils/keyboard.utils";
import { Chan } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";

export const settingsDropdown = ({
  alias,
  id,
  settings,
}: {
  alias: string;
  id: string;
  settings: { fn: () => void; caption: string; action: string }[];
}): TemplateResult => {
  /* Toggle and close the chasm for the attribute aria-expanded */
  const ariaExpanded = new Chan(false);
  const toggle = () => ariaExpanded.send(!ariaExpanded.latest);
  const close = () => ariaExpanded.send(false);

  const backdrop = ariaExpanded.map((expanded) =>
    expanded
      ? html`<div
          class="c-dropdown__backdrop"
          role="button"
          tabindex="-1"
          @click=${() => close()}
          @keypress=${(e: KeyboardEvent) =>
            handleKeyPress({ e, callback: close })}
        />`
      : undefined
  );

  return html` <div class="c-action-list__action c-dropdown">
    ${asyncReplace(backdrop)}
    <button
      class="c-dropdown__trigger c-action-list__action"
      aria-expanded=${asyncReplace(ariaExpanded)}
      aria-controls="dropdown-${id}"
      aria-label="Open settings"
      data-device=${alias}
      @click=${() => toggle()}
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
