import { PreLoadImage } from "$lib/utils/preLoadImage";
import { html, TemplateResult } from "lit-html";

export const identityCard = ({
  userNumber,
  identityBackground,
}: {
  userNumber: bigint;
  identityBackground: PreLoadImage;
}): TemplateResult => {
  return html`<div class="c-input--id__wrap">
    <img src=${identityBackground.getSrc()} class="c-input--id__art" />
    <h2 class="c-input--id__caption">Internet Identity:</h2>
    <output
      class="c-input--id__value"
      class="t-vip"
      aria-label="usernumber"
      id="userNumber"
      data-usernumber="${userNumber}"
      >${userNumber}</output
    >
  </div>`;
};
