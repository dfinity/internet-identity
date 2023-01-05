import { Challenge } from "../../../generated/internet_identity_types";
import { spinner } from "../../components/icons";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { html, render, TemplateResult } from "lit-html";
import { withRef, autofocus } from "../../utils/lit-html";
import { Chan } from "../../utils/utils";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { LoginFlowCanceled, cancel } from "../../utils/flowResult";
import {
  IdentifiableIdentity,
  Connection,
  RegisterResult,
} from "../../utils/iiConnection";

// A symbol that we can differentiate from generic `T` types
// when verifying the challenge
export const badChallenge: unique symbol = Symbol("ii.bad_challenge");

export const promptCaptchaTemplate = <T>(props: {
  cancel: () => void;
  requestChallenge: () => Promise<Challenge>;
  verifyChallengeChars: (cr: {
    chars: string;
    challenge: Challenge;
  }) => Promise<T | typeof badChallenge>;
  onContinue: (result: T) => void;
}) => {
  // We define a few Chans that are used to update the page in a
  // reactive way; see template returned by this function

  // The text and image shown
  const text = new Chan<TemplateResult>();
  const img = new Chan<TemplateResult>();

  // The text input where the chars can be typed
  const input: Ref<HTMLInputElement> = createRef();

  // The "next" button behavior
  const next = new Chan<((e: SubmitEvent) => void) | undefined>();
  const nextDisabled = next.map((f) => f === undefined);

  // The "retry" button behavior
  const retry = new Chan<(() => void) | undefined>();
  const retryDisabled = retry.map((f) => f === undefined);

  // The various states the component can inhabit
  type State =
    | { status: "requesting" }
    | { status: "prompting"; challenge: Challenge }
    | { status: "verifying" }
    | { status: "bad" };

  // On retry, request a new challenge
  const doRetry = async () => {
    update({ status: "requesting" });
    const challenge = await props.requestChallenge();
    update({ status: "prompting", challenge });
  };

  // On verification, check the chars and either continue (on good challenge)
  // or go to "bad" state
  const doVerify = async (challenge: Challenge) => {
    update({ status: "verifying" });
    withRef(input, async (input) => {
      const res = await props.verifyChallengeChars({
        chars: input.value,
        challenge,
      });
      res === badChallenge ? update({ status: "bad" }) : props.onContinue(res);
    });
  };

  // The update function, transitioning between states
  const update = (state: State) => {
    switch (state.status) {
      case "requesting":
        text.send(html`<p>Generating new challenge...</p>`);
        img.send(html`<div class="c-spinner">${spinner}</div> `);
        next.send(undefined);
        retry.send(undefined);
        break;
      case "prompting":
        text.send(html`<p>Type the characters you see</p>`);
        img.send(
          html`<img
            src="data:image/png;base64,${state.challenge.png_base64}"
            id="captchaImg"
            class="c-img-block l-stack"
            alt="captcha image"
          /> `
        );
        next.send((e) => {
          e.preventDefault();
          e.stopPropagation();
          doVerify(state.challenge);
        });
        retry.send(doRetry);
        break;
      case "verifying":
        text.send(html`<p>Verifying...</p>`);
        // omit updating `img` on purpose; we just leave whatever is shown (captcha)
        next.send(undefined);
        retry.send(undefined);
        break;
      case "bad":
        text.send(
          html`<p>
            The value you entered is incorrect. Click "retry" to generate a new
            value.
          </p>`
        );
        // omit updating `img` on purpose; we just leave whatever is shown (captcha)
        next.send(undefined);
        retry.send(doRetry);
        break;
    }
  };

  // Kickstart everything
  doRetry();

  return html`
    <article class="l-container c-card c-card--highlight">
      <h1 class="t-title t-title--main">Prove you're not a robot</h1>
      <form
        autocomplete="off"
        @submit=${asyncReplace(next.recv())}
        class="l-stack t-centered"
      >
        ${asyncReplace(text.recv())}
        <div class="c-input c-input--icon">
          ${asyncReplace(img.recv())}
          <i
            tabindex="0"
            id="seedCopy"
            class="c-button__icon c-input__icon"
            @click=${asyncReplace(retry.recv())}
            ?disabled=${asyncReplace(retryDisabled)}
          >
            <span>retry</span>
          </i>
        </div>
        <input ${autofocus} ${ref(input)} id="captchaInput" class="c-input" />
        <p class="t-paragraph confirm-paragraph"></p>
        <div class="c-button-group">
          <button
            type="button"
            @click=${() => props.cancel()}
            class="c-button c-button--secondary"
          >
            Cancel
          </button>
          <button
            type="submit"
            class="c-button"
            id="confirmRegisterButton"
            ?disabled=${asyncReplace(nextDisabled)}
          >
            Next
          </button>
        </div>
      </form>
    </article>
  `;
};

export const promptCaptchaPage = <T>(
  props: Parameters<typeof promptCaptchaTemplate<T>>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(promptCaptchaTemplate(props), contain);
};

export const promptCaptcha = ({
  connection,
  identity,
  alias,
  challenge,
}: {
  connection: Connection;
  identity: IdentifiableIdentity;
  alias: string;
  challenge?: Promise<Challenge>;
}): Promise<RegisterResult | LoginFlowCanceled> => {
  return new Promise((resolve) => {
    promptCaptchaPage({
      cancel: () => resolve(cancel),
      verifyChallengeChars: async ({ chars, challenge }) => {
        const result = await connection.register(identity, alias, {
          key: challenge.challenge_key,
          chars,
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
