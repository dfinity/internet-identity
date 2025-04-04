import { mainWindow } from "$lib/templates/mainWindow";
import { DynamicKey, I18n } from "$lib/legacy/i18n";
import { WrongCaptchaSolution } from "$lib/utils/iiConnection";
import { mount, renderPage, withRef } from "$lib/utils/lit-html";
import { Chan } from "$lib/utils/utils";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";
import copyJson from "./captcha.json";

export const promptCaptchaTemplate = <T>({
  cancel,
  captcha_png_base64,
  checkCaptcha,
  onContinue,
  i18n,
  focus: focus_,
  scrollToTop = false,
}: {
  cancel: () => void;
  captcha_png_base64: string;
  checkCaptcha: (
    solution: string,
  ) => Promise<Exclude<T, WrongCaptchaSolution> | WrongCaptchaSolution>;
  onContinue: (result: Exclude<T, WrongCaptchaSolution>) => void;
  i18n: I18n;
  focus?: boolean;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const focus = focus_ ?? false;
  const copy = i18n.i18n(copyJson);

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
    | { status: "prompting"; captcha_png_base64: string }
    | { status: "verifying" }
    | { status: "bad" };

  // We define a few Chans that are used to update the page in a
  // reactive way based on state; see template returned by this function
  const state = new Chan<State>({ status: "prompting", captcha_png_base64 });

  // The image shown
  const img: Chan<TemplateResult> = state.map({
    f: (state) =>
      state.status === "prompting"
        ? captchaImg(state.captcha_png_base64)
        : Chan.unchanged,
    def: captchaImg(captcha_png_base64),
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
      })[status],
    def: undefined,
  });

  const hasError = errorText.map((errorText) =>
    nonNullish(errorText) ? "has-error" : "",
  );

  // The "next" button behavior
  const next: Chan<((e: SubmitEvent) => void) | undefined> = state.map(
    (state) =>
      state.status === "prompting"
        ? (e) => {
            e.preventDefault();
            e.stopPropagation();
            doVerify();
          }
        : undefined,
  );

  const nextDisabled: Chan<boolean> = next.map(isNullish);
  const nextCaption: Chan<DynamicKey> = state.map(({ status }) => {
    if (status === "verifying") {
      return copy.verifying;
    }
    return copy.next;
  });

  // On retry, prompt with a new challenge
  // On verification, check the chars and either continue (on good challenge)
  // or go to "bad" state
  const doVerify = () => {
    state.send({ status: "verifying" });
    void withRef(input, async (input) => {
      const res = await checkCaptcha(input.value);
      if (isBadCaptchaResult(res)) {
        // on a bad challenge, show some error, clear the input & focus
        // and retry
        state.send({ status: "bad" });
        input.value = "";
        input.focus();
        state.send({
          status: "prompting",
          captcha_png_base64: res.new_captcha_png_base64,
        });
        return;
      }
      onContinue(res);
    });
  };

  // A "resize" handler than ensures that the captcha is centered when after
  // the page is resized. This is particularly useful on mobile devices, where
  // the virtual keyboard shifts the content up or down.
  //
  // The handler automatically deregisters itself the first time it is called when
  // the captcha doesn't exist anymore.
  //
  // Should not be registered before the template is rendered.
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
        </div>
        <label>
          <strong class="t-strong">${copy.instructions}</strong>
          <input
            ?autofocus=${focus}
            ${ref(input)}
            id="captchaInput"
            class="c-input c-input--stack c-input--fullwidth ${asyncReplace(
              hasError,
            )}"
            autocapitalize="none"
            spellcheck="false"
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
  container?: HTMLElement,
): void {
  return renderPage<(props: TemplateProps<T>) => TemplateResult>(
    promptCaptchaTemplate,
  )(props, container);
}

export const promptCaptcha = <T>({
  captcha_png_base64,
  checkCaptcha,
}: {
  captcha_png_base64: string;
  checkCaptcha: (
    solution: string,
  ) => Promise<Exclude<T, WrongCaptchaSolution> | WrongCaptchaSolution>;
}): Promise<Exclude<T, WrongCaptchaSolution> | "canceled"> => {
  return new Promise((resolve) => {
    const i18n = new I18n();
    promptCaptchaPage<T>({
      cancel: () => resolve("canceled"),
      captcha_png_base64,
      checkCaptcha,
      onContinue: resolve,
      i18n,
      scrollToTop: true,
      focus: true,
    });
  });
};

const isBadCaptchaResult = <T>(
  res: Exclude<T, WrongCaptchaSolution> | WrongCaptchaSolution,
): res is WrongCaptchaSolution => {
  return (
    nonNullish(res) &&
    typeof res === "object" &&
    "kind" in res &&
    res.kind === "wrongCaptchaSolution"
  );
};

// Returns a function that returns `first` on the first call,
// and values returned by `f()` from the second call on.
export function precomputeFirst<T>(f: () => T): () => T {
  let firstTime = true;
  const first: T = f();

  return () => {
    if (firstTime) {
      firstTime = false;
      return first;
    } else {
      return f();
    }
  };
}
