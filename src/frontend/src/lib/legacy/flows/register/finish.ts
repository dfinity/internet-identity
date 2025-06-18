import identityCardBackground from "$lib/legacy/assets/identityCardBackground.png?url";
import { checkmarkIcon, copyIcon } from "$lib/templates/icons";
import { identityCard } from "$lib/templates/identityCard";
import { mainWindow } from "$lib/templates/mainWindow";
import { toast } from "$lib/templates/toast";
import {
  RegistrationEvents,
  registrationFunnel,
} from "$lib/utils/analytics/registrationFunnel";
import {
  TemplateElement,
  mount,
  renderPage,
  withRef,
} from "$lib/utils/lit-html";
import { PreLoadImage } from "$lib/utils/preLoadImage";
import { OmitParams } from "$lib/utils/utils";
import { TemplateResult, html } from "lit-html";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";

export const displayUserNumberTemplate = ({
  onContinue,
  userNumber,
  identityBackground,
  marketingIntroSlot,
  scrollToTop = false,
}: {
  onContinue: () => void;
  userNumber: bigint;
  identityBackground: PreLoadImage;
  marketingIntroSlot?: TemplateResult;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const userNumberCopy: Ref<HTMLButtonElement> = createRef();
  const displayUserNumberSlot = html`
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
    <div class="c-input c-input--stack c-input--textarea c-input--readonly c-input--icon c-input--id">
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
            registrationFunnel.trigger(
              RegistrationEvents.CopyNewIdentityNumber,
            );
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
        <h3 class="t-title">Why is it important to save this number?</h3>
        <p class="t-paragraph">This number is unique but not secret. If you lose this number, you will lose access to
          all of the accounts that you created with it</p>
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
  const identityBackground = new PreLoadImage(identityCardBackground);
  return async (opts) => {
    await displayUserNumber({ ...opts, identityBackground });
  };
};

export const displayUserNumber = ({
  userNumber,
  identityBackground,
  marketingIntroSlot,
}: {
  userNumber: bigint;
  identityBackground: PreLoadImage;
  marketingIntroSlot?: TemplateResult;
}): Promise<void> => {
  return new Promise((resolve) =>
    displayUserNumberPage({
      onContinue: () => resolve(),
      userNumber,
      identityBackground,
      marketingIntroSlot,
      scrollToTop: true,
    }),
  );
};
