import { checkmarkIcon, copyIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { BASE_URL } from "$src/environment";
import { renderPage, withRef } from "$src/utils/lit-html";
import { html } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

export const displayUserNumberTemplate = ({
  onContinue,
  userNumber,
  identityBackground: identityBackground_,
}: {
  onContinue: () => void;
  userNumber: bigint;
  identityBackground?: IdentityBackground;
}) => {
  const userNumberCopy: Ref<HTMLButtonElement> = createRef();
  const identityBackground = identityBackground_ ?? new IdentityBackground();
  const displayUserNumberSlot = html`<hgroup>
      <h1 class="t-title t-title--main">
        You’ve created an Internet Identity!
      </h1>
      <p class="t-paragraph">
        Save this number by taking a screenshot or writing it down.
      </p>
    </hgroup>
    <div class="c-input c-input--textarea c-input--readonly c-input--icon c-input--id" >
      <div class="c-input--id__wrap">
      ${identityBackground.img}
        <h2 class="c-input--id__caption">Internet Identity:</h2>
        <output class="c-input--id__value" class="t-vip" aria-label="usernumber" id="userNumber" data-usernumber="${userNumber}">${userNumber}</output>
      </div>
        <button
          ${ref(userNumberCopy)}
          aria-label="Copy phrase to clipboard""
          title="Copy phrase to clipboard"
          tabindex="0"
          class="c-button__icon"
          @click=${async () => {
            try {
              await navigator.clipboard.writeText(userNumber.toString());
              withRef(userNumberCopy, (elem) => {
                elem.classList.add("is-copied");
              });
            } catch (e: unknown) {
              toast.error("Unable to copy Internet Identity");
              console.error("Unable to copy Internet Identity", e);
            }
          }}
          >
            <span>Copy</span>
            ${copyIcon}
            ${checkmarkIcon}
          </button>
    </div>
      <button
        @click=${() => onContinue()}
        id="displayUserContinue"
        class="c-button l-stack"
      >
        I saved it, continue
      </button>
    <section class="c-marketing-block">
      <aside class="l-stack">
        <h3 class="t-title">What is this number?</h3>
        <ul class="c-list c-list--bulleted">
          <li>Your Internet Identity</li>
          <li>With your Internet Identity and your passkey, you will be able to create and sign into private accounts on IC dapps</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Why is it important to save this number?</h3>
        <ul class="c-list c-list--bulleted">
          <li>If you lose this number, you will lose access to all of the accounts that you created with it</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Is this number secret?</h3>
        <ul class="c-list c-list--bulleted">
          <li>No, this number is unique to you but it is not secret</li>
        </ul>
      </aside>
    </section>

    `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: displayUserNumberSlot,
  });
};

// A non-exported wrapper around HTMLImageElement, to ensure this exact image is loaded
// (and no other image is passed as an argument to this page)
class IdentityBackground {
  public img: HTMLImageElement;

  constructor() {
    const img = new Image();
    img.src = `${BASE_URL}image.png`; // Setting the src kicks off the fetching
    img.classList.add("c-input--id__art");
    img.alt = "";
    this.img = img;
  }
}

export const displayUserNumberPage = renderPage(displayUserNumberTemplate);

// Omit specified functions parameters, for instance OmitParams<..., "foo" | "bar">
// will transform
//  f: (a: { foo, bar, baz }) => void
// into
//  f: (a: { baz }) => void
//
// eslint-disable-next-line
type OmitParams<T extends (arg: any) => any, A extends string> = (
  a: Omit<Parameters<T>[0], A>
) => ReturnType<T>;

// A variant of `displayUserNumber` where the `identityBackground` is preloaded
export const displayUserNumberWarmup = (): OmitParams<
  typeof displayUserNumber,
  "identityBackground"
> => {
  const identityBackground = new IdentityBackground();
  return async (opts) => {
    await displayUserNumber({ ...opts, identityBackground });
  };
};

export const displayUserNumber = ({
  userNumber,
  identityBackground,
}: {
  userNumber: bigint;
  identityBackground?: IdentityBackground;
}): Promise<void> => {
  return new Promise((resolve) =>
    displayUserNumberPage({
      onContinue: () => resolve(),
      userNumber,
      identityBackground,
    })
  );
};
