import { html, nothing, render, TemplateResult } from "lit-html";
import { DeviceData } from "../../../../generated/internet_identity_types";
import {
  apiResultToLoginFlowResult,
  LoginFlowCanceled,
  canceled,
  LoginFlowSuccess,
} from "../../login/flowResult";
import { dropLeadingUserNumber } from "../../../crypto/mnemonic";
import { IIConnection } from "../../../utils/iiConnection";
import { displayError } from "../../../components/displayError";
import { unreachable } from "../../../utils/utils";
import {
  Warning,
  RECOVERYPHRASE_WORDCOUNT,
  getWarnings,
} from "../../../crypto/mnemonic";
import { warningIcon } from "../../../components/icons";
import { questions } from "../../faq";

const pageContent = (userNumber: bigint) => html`
  <style>

    /* Flash the warnings box if warnings were generated */
    @keyframes flash-warnings {
      0% {
        background-color: unset;
      }
      50% {
        background-color: #ED1E79;
        border-color: #ED1E79;
      }
      100% {
        background-color: unset;
      }
    }

    .visible {
      visibility: unset;
      animation-name: flash-warnings;
      animation-duration: 600ms;
    }

    .hidden {
      visibility: hidden;
    }

    #inputSeedPhrase {
      width: 100%;
      height: 6rem;
      box-sizing: border-box;
      margin-bottom: 0;
      font-size: 1rem;
      font-weight: 400;
    }

    .full-width {
      width: 100%;
    }

    .warnings-box {
      border: 1px var(--grey-200) solid;
      border-radius: 4px;
      display: flex;
      align-items: center;
      gap: 1rem;
      padding: 1rem;
      margin-top: 0.5rem;
      margin-bottom: 0.5rem;
    }

    .warnings-box-summary {
        padding-left: 1em;
    }

    .warning {
        display: flex;
        align-items: center;
        gap: 1rem;
        padding: 0.5rem;
    }

    .warning-message {
        display: flex;
        align-items: center;
    }

    .warning-message p {
        margin: 0;
    }

  </style>
  <div class="container full-width">
    <h1>Your seed phrase</h1>
    <p>Please provide your seed phrase</p>
    <textarea id="inputSeedPhrase" placeholder="${
      userNumber + " above squirrel ..."
    }"></textarea>
    <details class="warnings-box hidden">
        <summary><span class="warnings-box-summary">Phrase may not be valid<span></summary>
        <div id="warnings"></div>
    </details>
    <button id="inputSeedPhraseContinue" class="primary">Continue</button>
    <button id="inputSeedPhraseCancel">Cancel</button>
  </div>
`;

export const phraseRecoveryPage = async (
  userNumber: bigint,
  device: DeviceData,
  prefilledPhrase?: string
): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(userNumber, device, prefilledPhrase);
};

const init = (
  userNumber: bigint,
  device: DeviceData,
  prefilledPhrase?: string /* if set, prefilled as input */
): Promise<LoginFlowSuccess | LoginFlowCanceled> =>
  new Promise((resolve) => {
    const inputSeedPhraseInput = document.getElementById(
      "inputSeedPhrase"
    ) as HTMLInputElement;

    // Look up the warningsDiv element early; this must not be done inside in the warnings
    // generation callback since -- due to debouncing -- it may be called after some period of
    // time and the user may have left the page; we may effectively pick up the wrong element.
    const warningsDiv = document.getElementById("warnings") as HTMLDivElement;
    const warningsBox = document.querySelector(
      "details.warnings-box"
    ) as HTMLDetailsElement;

    // Debounce the warning generation as not to spam the user
    let handle: number;
    const debouncedWarnings = () => {
      clearTimeout(handle);
      handle = window.setTimeout(() => {
        warningsDiv.innerHTML = ""; // Clear previously generated warnings

        const warnings = getWarningMessages(
          userNumber,
          inputSeedPhraseInput.value.trim()
        );

        if (warnings.length >= 1) {
          warningsBox.classList.add("visible");
          warningsBox.classList.remove("hidden");

          // Actually generate the warnings
          for (const warning of warnings) {
            const div = document.createElement("div");
            render(mkWarningDiv(warning), div);
            warningsDiv.appendChild(div);
          }
        } else {
          warningsBox.classList.add("hidden");
          warningsBox.classList.remove("visible");
        }
      }, 500);
    };

    if (prefilledPhrase !== undefined) {
      inputSeedPhraseInput.value = prefilledPhrase;
    }
    inputSeedPhraseInput.oninput = () => {
      debouncedWarnings();
    };
    const inputSeedPhraseContinue = document.getElementById(
      "inputSeedPhraseContinue"
    ) as HTMLButtonElement;
    const inputSeedPhraseCancel = document.getElementById(
      "inputSeedPhraseCancel"
    ) as HTMLButtonElement;
    inputSeedPhraseCancel.onclick = () => {
      resolve(canceled());
    };
    inputSeedPhraseContinue.onclick = async () => {
      const inputValue = inputSeedPhraseInput.value.trim();
      const mnemonic = dropLeadingUserNumber(inputValue);
      const result = apiResultToLoginFlowResult(
        await IIConnection.fromSeedPhrase(userNumber, mnemonic, device)
      );

      switch (result.tag) {
        case "ok":
          resolve(result);
          break;
        case "err":
          await displayError({ ...result, primaryButton: "Try again" });
          phraseRecoveryPage(userNumber, device, inputValue).then((res) =>
            resolve(res)
          );
          break;
        default:
          unreachable(result);
          break;
      }
    };
  });

export const getWarningMessages = (
  userNumber: bigint,
  input: string
): (TemplateResult | string)[] => {
  return getWarnings(userNumber, input).map((warning) =>
    warningMessage(userNumber, warning)
  );
};

/** The warning messages, for each warning type. */
export const warningMessage = (
  userNumber: bigint,
  warning: Warning
): TemplateResult | string => {
  switch (warning.type) {
    case "bad_chars": {
      return html`Unexpected character${warning.chars.length > 1 ? "s" : ""}:
      ${warning.chars.map((c, i) => [
        html`<code>${c === "\n" ? "newline" : c === "\t" ? "tab" : c}</code>`,
        i < warning.chars.length - 1 ? ", " : nothing,
      ])}`;
    }

    case "repeated_whitespace": {
      return html`Multiple
      whitespaces${warning.between
        ? html` between <code>${warning.between[0]}</code> and
            <code>${warning.between[1]}</code>`
        : ""}`;
    }

    case "bad_anchor": {
      return html`Recovering anchor <strong>${userNumber}</strong>, but recovery
        phrase suggests anchor <strong>${warning.anchor}</strong>`;
    }

    case "bad_word_count": {
      return `Recovery phrase should contain ${RECOVERYPHRASE_WORDCOUNT} words, but input contains ${
        warning.count
      } word${warning.count > 1 ? "s" : ""}.`;
    }

    case "bad_words": {
      return html`Unexpected word${warning.words.length > 1 ? "s" : ""}:
      ${warning.words.map((word, i) => [
        html`<code>${word}</code>`,
        i < warning.words.length - 1 ? ", " : nothing,
      ])}`;
    }

    case "invalid": {
      return html`
        This does not look like a seed phrase generated by Internet Identity,
        please make sure to copy the full seedphrase and try again. For more
        information, please see
        <a target="_blank" href="/faq#${questions.invalidSeedphrase.anchor}"
          >${questions.invalidSeedphrase.question}</a
        >
      `;
    }
  }
};

const mkWarningDiv = (warningMessage: string | TemplateResult) => html` <div
  class="warning"
>
  <span class="warningIcon">${warningIcon}</span>
  <div class="warning-message">
    <p>${warningMessage}</p>
  </div>
</div>`;
