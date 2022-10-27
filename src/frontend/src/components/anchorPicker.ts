import { html, TemplateResult } from "lit-html";
import { arrowRight } from "./icons";
import { mount } from "../utils/lit-html";
import { mkAnchorInput } from "./anchorInput";
import { until } from "lit-html/directives/until.js";

type PickerProps = {
  savedAnchors: bigint[];
  pick: PickCB;
};

type PickCB = (userNumber: bigint) => void;

/** A component for picking an anchor number. One big list where the first
 * elements are (clickable) anchors, and the last (and sometimes only) element
 * is a menu for using any anchor or creating a new anchor. */
export const mkAnchorPicker = (
  props: PickerProps
): {
  template: TemplateResult;
} => {
  const template = html`<ul class="c-list c-list--anchors l-stack">
    ${elements(props)}
  </ul>`;

  return { template };
};

// The list elements, including the anchor creation menu
function elements(props: PickerProps): TemplateResult[] {
  const otherAnchorMenuTpl = otherAnchorMenu({ pick: props.pick });
  const savedAnchorsTpl = props.savedAnchors.map((anchor, i) =>
    anchorItem({ anchor, pick: props.pick, focus: i == 0 })
  );

  let elems = [];

  if (savedAnchorsTpl.length > 0) {
    // If there are saved anchors, show those, and then an expandable menu
    // for "other anchor".

    // Function that replaces the "other anchor" button with the "other anchor" menu
    // (actually a reference written when the template is created)
    let otherAnchorOpen: (() => void) | undefined = undefined;

    const otherAnchorOffer: TemplateResult = html`
      <button
        class="t-link c-list__parcel c-list__parcel--fullwidth c-list__parcel--summary"
        @click="${() => {
          otherAnchorOpen?.();
        }}"
        @focus="${() => {
          otherAnchorOpen?.();
        }}"
        data-role="use-another-anchor"
      >
        Use another anchor<i class="c-list__icon"> … </i>
      </button>
    `;

    elems = savedAnchorsTpl;
    elems.push(html` <li class="c-list__item c-list__item--noFocusStyle">
      ${until(
        // replace the "use another anchor" button with actual menu when ready (clicked)
        new Promise<void>((resolve) => {
          otherAnchorOpen = resolve;
        }).then(() => Promise.resolve(otherAnchorMenu({ pick: props.pick }))),
        otherAnchorOffer
      )}
    </li>`);
  } else {
    // If there are no saved anchors, just show the (expanded) "other anchor" menu.
    elems.push(
      html` <li class="c-list__item c-list__item--noFocusStyle">
        ${otherAnchorMenuTpl}
      </li>`
    );
  }

  return elems;
}

const anchorItem = (props: {
  anchor: bigint;
  pick: PickCB;
  focus: boolean /* when 'true' the element will be focused when created */;
}): TemplateResult => html`
  <li class="c-list__item c-list__item--vip c-list__item--icon icon-trigger">
    <button
      ${props.focus
        ? mount((e) => {
            if (e instanceof HTMLElement) {
              e.focus();
            }
          })
        : ""}
      data-anchor-id=${props.anchor}
      class="c-list__parcel c-list__parcel--select"
      @click="${() => props.pick(props.anchor)}"
      tabindex="0"
    >
      ${props.anchor}
    </button>
    <i class="c-list__icon c-list__icon--masked"> ${arrowRight} </i>
  </li>
`;

function otherAnchorMenu(props: { pick: PickCB }): TemplateResult {
  const anchorInput = mkAnchorInput({ onSubmit: props.pick });

  return html`
    <div
      class="c-list__parcel c-list__parcel--fullwidth c-list__parcel--detail"
    >
      ${anchorInput.template}
      <button
        class="c-button"
        data-role="anchor-create"
        @click="${anchorInput.submit}"
      >
        Oh ouiiii
      </button>
      <p class="l-stack">
        An <b class="t-strong">Identity Anchor</b> is a
        <b class="t-strong">unique ID</b> that is used to authenticate yourself.
        You will be able to use it to <b class="t-strong">log in</b> to all
        kinds of apps.
      </p>
      <div class="l-stack">
        <button class="c-button c-button--secondary">
          Create a new anchor
        </button>
      </div>
    </div>
  `;
}
