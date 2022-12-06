import { html, TemplateResult } from "lit-html";
import { arrowRight } from "./icons";
import { autofocus } from "../utils/lit-html";
import { NonEmptyArray } from "../utils/utils";

type PickerProps = {
  savedAnchors: NonEmptyArray<bigint>;
  pick: PickCB;
  moreOptions: () => void;
};

type PickCB = (userNumber: bigint) => void;

/** A component for picking an anchor number. One big list where the elements are (clickable)
 * anchors */
export const mkAnchorPicker = (
  props: PickerProps
): {
  template: TemplateResult;
} => {
  const elems = props.savedAnchors.map((anchor, i) =>
    anchorItem({ anchor, pick: props.pick, focus: i === 0 })
  );

  const moreOptions = html` <li class="c-list__item c-list__item--noFocusStyle">
    <button
      class="t-link c-list__parcel c-list__parcel--fullwidth c-list__parcel--summary"
      @click="${() => props.moreOptions()}"
      data-role="more-options"
    >
      More options<i class="c-list__icon"> … </i>
    </button>
  </li>`;

  const template = html`<ul class="c-list c-list--anchors l-stack">
    ${[...elems, moreOptions]}
  </ul>`;

  return { template };
};

const anchorItem = (props: {
  anchor: bigint;
  pick: PickCB;
  focus: boolean /* when 'true' the element will be focused when created */;
}): TemplateResult => html`
  <li class="c-list__item c-list__item--vip c-list__item--icon icon-trigger">
    <button
      ${props.focus ? autofocus : undefined}
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
