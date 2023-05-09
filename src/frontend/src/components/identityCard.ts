import { BASE_URL } from "$src/environment";
import { html, TemplateResult } from "lit-html";

export const identityCard = ({
  userNumber,
  identityBackground,
}: {
  userNumber: bigint;
  identityBackground: IdentityBackground;
}): TemplateResult => {
  return html` <div class="c-input--id__wrap">
    <img src=${identityBackground.img.src} class="c-input--id__art" />
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

// A wrapper around HTMLImageElement, to ensure this exact image is loaded
// (and no other image is passed as an argument to this page by mistake)
export class IdentityBackground {
  // The image element created in order for the browser to load the image. Only the src attribute is used.
  public img: HTMLImageElement;

  // Only the src attribute is used so the same Element can be shared across Templates
  public static singleton?: IdentityBackground;
  public static getSingleton(): IdentityBackground {
    IdentityBackground.singleton ??= new IdentityBackground();
    return IdentityBackground.singleton;
  }

  constructor() {
    const img = new Image();
    img.src = `${BASE_URL}image.png`; // Setting the src kicks off the fetching
    this.img = img;
  }
}

// Start loading the card background; should ideally be called a couple seconds
// before rendering the template
export const loadIdentityBackground = IdentityBackground.getSingleton;
