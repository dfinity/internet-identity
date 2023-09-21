import { checkmarkIcon, copyIcon } from "$src/components/icons";
import {
  IdentityBackground,
  identityCard,
  loadIdentityBackground,
} from "$src/components/identityCard";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import {
  TemplateElement,
  mount,
  renderPage,
  withRef,
} from "$src/utils/lit-html";
import { OmitParams } from "$src/utils/utils";
import { TemplateResult, html } from "lit-html";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";

export const displayUserNumberTemplate = ({
  onContinue,
  userNumber,
  identityBackground,
  stepper,
  marketingIntroSlot,
  scrollToTop = false,
}: {
  onContinue: () => void;
  userNumber: bigint;
  identityBackground: IdentityBackground;
  stepper: TemplateResult;
  marketingIntroSlot?: TemplateResult;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const userNumberCopy: Ref<HTMLButtonElement> = createRef();
  const displayUserNumberSlot = html`

  ${stepper}
<hgroup

      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
  >

      <h1 class="t-title t-title--main">
        You’ve created an Internet Identity!
      </h1>
      <p class="t-paragraph">
        Save this number by taking a screenshot or writing it down.
      </p>
    </hgroup>
    <div class="c-input c-input--stack c-input--textarea c-input--readonly c-input--icon c-input--id" >
      ${
        identityCard({
          userNumber,
          identityBackground,
        }) satisfies TemplateElement
      }
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
      ${marketingIntroSlot}
      <aside class="l-stack">
        <h3 class="t-title">This number is your Internet Identity</h3>
        <p class="t-paragraph">With your Internet Identity and your passkey, you will be able to create and securely connect to Internet Computer dapps</p>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Why is it important to save this number?</h3>
        <p class="t-paragraph">If you lose this number, you will lose access to all of the accounts that you created with it</paragraph>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Is this number secret?</h3>
        <p class="t-paragraph"> No, this number is unique to you but it is not secret</p>
      </aside>
    </section>

    `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: displayUserNumberSlot,
  });
};

export const displayUserNumberPage = renderPage(displayUserNumberTemplate);

// A variant of `displayUserNumber` where the `identityBackground` is preloaded
export const displayUserNumberWarmup = (): OmitParams<
  typeof displayUserNumber,
  "identityBackground"
> => {
  const identityBackground = loadIdentityBackground();
  return async (opts) => {
    await displayUserNumber({ ...opts, identityBackground });
  };
};

export const displayUserNumber = ({
  userNumber,
  identityBackground,
  stepper,
  marketingIntroSlot,
}: {
  userNumber: bigint;
  identityBackground: IdentityBackground;
  stepper: TemplateResult;
  marketingIntroSlot?: TemplateResult;
}): Promise<void> => {
  return new Promise((resolve) =>
    displayUserNumberPage({
      onContinue: () => resolve(),
      userNumber,
      identityBackground,
      stepper,
      marketingIntroSlot,
      scrollToTop: true,
    })
  );
};
