import { Challenge } from "$generated/internet_identity_types";
import { mainWindow } from "$src/components/mainWindow";
import { DynamicKey, I18n } from "$src/i18n";
import { cancel, LoginFlowCanceled } from "$src/utils/flowResult";
import {
  Connection,
  IIWebAuthnIdentity,
  RegisterResult,
} from "$src/utils/iiConnection";
import { autofocus, mount, renderPage, withRef } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { registerStepper } from "./stepper";

import { isNullish, nonNullish } from "@dfinity/utils";
import copyJson from "./captcha.json";

// A symbol that we can differentiate from generic `T` types
// when verifying the challenge
export const badChallenge: unique symbol = Symbol("ii.bad_challenge");

export const promptCaptchaTemplate = <T>({
  cancel,
  requestChallenge,
  verifyChallengeChars,
  onContinue,
  i18n,
  focus: focus_,
  scrollToTop = false,
}: {
  cancel: () => void;
  requestChallenge: () => Promise<Challenge>;
  verifyChallengeChars: (cr: {
    chars: string;
    challenge: Challenge;
  }) => Promise<T | typeof badChallenge>;
  onContinue: (result: T) => void;
  i18n: I18n;
  focus?: boolean;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const focus = focus_ ?? true;
  const copy = i18n.i18n(copyJson);

  const spinnerImg: TemplateResult = html`
    <div
      class="c-captcha-placeholder c-spinner-wrapper"
      aria-label="Loading image"
    >
      <div class="c-spinner">
        <i class="c-spinner__inner"></i>
      </div>
    </div>
  `;
  const captchaImg = (base64: string): TemplateResult =>
    html`<div class="c-captcha-placeholder" aria-label="CAPTCHA challenge">
      <img
        src="data:image/png;base64,${base64}"
        class="c-image"
        alt="CAPTCHA Characters"
      />
    </div>`;

  // The various states the component can inhabit
  type State =
    | { status: "requesting" }
    | { status: "prompting"; challenge: Challenge }
    | { status: "verifying" }
    | { status: "bad" };

  // We define a few Chans that are used to update the page in a
  // reactive way based on state; see template returned by this function
  const state = new Chan<State>({ status: "requesting" });

  // The image shown
  const img: Chan<TemplateResult> = state.map({
    f: (state) =>
      state.status === "requesting"
        ? spinnerImg
        : state.status === "prompting"
        ? captchaImg(state.challenge.png_base64)
        : Chan.unchanged,
    def: spinnerImg,
  });

  // The text input where the chars can be typed
  const input: Ref<HTMLInputElement> = createRef();

  // The CAPTCHA container
  const captchaContainer: Ref<HTMLElement> = createRef();

  // The error shown on bad input
  const errorText: Chan<DynamicKey | undefined> = state.map({
    f: ({ status }) =>
      ({
        requesting: Chan.unchanged,
        prompting: Chan.unchanged,
        bad: copy.incorrect,
        verifying: undefined,
      }[status]),
    def: undefined,
  });

  const hasError = errorText.map((errorText) =>
    nonNullish(errorText) ? "has-error" : ""
  );

  // The "next" button behavior
  const next: Chan<((e: SubmitEvent) => void) | undefined> = state.map(
    (state) =>
      state.status === "prompting"
        ? (e) => {
            e.preventDefault();
            e.stopPropagation();
            doVerify(state.challenge);
          }
        : undefined
  );

  const nextDisabled: Chan<boolean> = next.map(isNullish);
  const nextCaption: Chan<DynamicKey> = state.map(({ status }) =>
    status === "requesting"
      ? copy.generating
      : status === "verifying"
      ? copy.verifying
      : copy.next
  );

  // The "retry" button behavior
  const retry: Chan<(() => Promise<void>) | undefined> = state.map((state) =>
    state.status === "prompting" || state.status === "bad" ? doRetry : undefined
  );
  const retryDisabled: Chan<boolean> = retry.map(isNullish);

  // On retry, request a new challenge
  const doRetry = async () => {
    state.send({ status: "requesting" });
    const challenge = await requestChallenge();
    state.send({ status: "prompting", challenge });
  };

  // On verification, check the chars and either continue (on good challenge)
  // or go to "bad" state
  const doVerify = (challenge: Challenge) => {
    state.send({ status: "verifying" });
    void withRef(input, async (input) => {
      const res = await verifyChallengeChars({
        chars: input.value,
        challenge,
      });
      if (res === badChallenge) {
        // on a bad challenge, show some error, clear the input
        // and retry
        state.send({ status: "bad" });
        input.value = "";
        void doRetry();
      } else {
        onContinue(res);
      }
    });
  };

  // Kickstart everything
  void doRetry();

  // A "resize" handler than ensures that the captcha is centered when after
  // the page is resized. This is particularly useful on mobile devices, where
  // the virtual keyboard shifts the content up or down.
  //
  // The handler automatically deregisters itself the first time it is called when
  // the captcha doesn't exist anymore.
  //
  // Should not be registerd before the template is rendered.
  const setResizeHandler = () => {
    const value = captchaContainer.value;
    if (isNullish(value)) {
      window.visualViewport?.removeEventListener("resize", setResizeHandler);
    } else {
      value.scrollIntoView();
    }
  };

  const promptCaptchaSlot = html`
    <article ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      ${registerStepper({ current: "captcha" })}
      <h1 class="t-title t-title--main">${copy.title}</h1>
      <form autocomplete="off" @submit=${asyncReplace(next)} class="l-stack">
        <div
          ${ref(captchaContainer)}
          ${mount(() => {
            window.visualViewport?.addEventListener("resize", setResizeHandler);
          })}
          class="c-input c-input--icon"
        >
          ${asyncReplace(img)}
          <i
            tabindex="0"
            id="seedCopy"
            class="c-button__icon"
            @click=${asyncReplace(retry)}
            ?disabled=${asyncReplace(retryDisabled)}
          >
            <span>${copy.retry}</span>
          </i>
        </div>
        <label>
          <strong class="t-strong">${copy.instructions}</strong>
          <input
            ${focus ? autofocus : undefined}
            ${ref(input)}
            id="captchaInput"
            class="c-input ${asyncReplace(hasError)}"
          />
          <strong class="c-input__message">${asyncReplace(errorText)}</strong>
        </label>
        <p class="t-paragraph confirm-paragraph"></p>
        <div class="c-button-group">
          <button
            type="button"
            @click=${() => cancel()}
            class="c-button c-button--secondary"
          >
            ${copy.cancel}
          </button>
          <button
            type="submit"
            class="c-button"
            id="confirmRegisterButton"
            ?disabled=${asyncReplace(nextDisabled)}
          >
            ${asyncReplace(nextCaption)}
          </button>
        </div>
      </form>
    </article>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: promptCaptchaSlot,
  });
};

type TemplateProps<T> = Parameters<typeof promptCaptchaTemplate<T>>[0];

export function promptCaptchaPage<T>(
  props: TemplateProps<T>,
  container?: HTMLElement
): void {
  return renderPage<(props: TemplateProps<T>) => TemplateResult>(
    promptCaptchaTemplate
  )(props, container);
}

export const promptCaptcha = ({
  connection,
  identity,
  alias,
  challenge,
}: {
  connection: Connection;
  identity: IIWebAuthnIdentity;
  alias: string;
  challenge?: Promise<Challenge>;
}): Promise<RegisterResult | LoginFlowCanceled> => {
  return new Promise((resolve) => {
    const i18n = new I18n();
    promptCaptchaPage({
      cancel: () => resolve(cancel),
      verifyChallengeChars: async ({ chars, challenge }) => {
        const tempIdentity = await ECDSAKeyIdentity.generate({
          extractable: false,
        });
        const result = await connection.register({
          identity,
          tempIdentity,
          alias,
          challengeResult: {
            key: challenge.challenge_key,
            chars,
          },
        });

        switch (result.kind) {
          case "badChallenge":
            return badChallenge;
          default:
            return result;
        }
      },

      requestChallenge: precomputedFirst(
        // For the first call, use a pre-generated challenge
        // if available.
        challenge ?? connection.createChallenge(),
        () => connection.createChallenge()
      ),

      onContinue: resolve,
      i18n,
      scrollToTop: true,
    });
  });
};

// Returns a function that returns `first` on the first call,
// and values returned by `f()` from the second call on.
function precomputedFirst<T>(first: T, f: () => T): () => T {
  let firstTime = true;

  return () => {
    if (firstTime) {
      firstTime = false;
      return first;
    } else {
      return f();
    }
  };
}
