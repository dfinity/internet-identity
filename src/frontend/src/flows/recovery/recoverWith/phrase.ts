import { html, nothing, render, TemplateResult } from "lit-html";
import { DeviceData } from "../../../../generated/internet_identity_types";
import {
  apiResultToLoginFlowResult,
  LoginFlowCanceled,
  cancel,
  LoginFlowSuccess,
} from "../../../utils/flowResult";
import { dropLeadingUserNumber } from "../../../crypto/mnemonic";
import { Connection } from "../../../utils/iiConnection";
import { displayError } from "../../../components/displayError";
import { unreachable } from "../../../utils/utils";
import {
  Warning,
  RECOVERYPHRASE_WORDCOUNT,
  getWarnings,
} from "../../../crypto/mnemonic";
import { warningIcon } from "../../../components/icons";
import { questions } from "../../faq";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = (userNumber: bigint, message?: string) => mainWindow({
  showLogo: false,
  showFooter: false,
  slot: html`
  <style>
    /* Flash the warnings box if warnings were generated */
    @keyframes flash-warnings {
      0% {
        background-color: unset;
      }
      50% {
        background-color: #ed1e79;
        border-color: #ed1e79;
      }
      100% {
        background-color: unset;
      }
    }

    .is-visible {
      animation-name: flash-warnings;
      animation-duration: 600ms;
    }
  </style>
  <hgroup>
    <h1 class="t-title t-title--main">Your seed phrase</h1>
    <p class="t-lead">
      ${message !== undefined ? message : "Please provide your seed phrase"}
    </p>
  </hgroup>
  <textarea
    id="inputSeedPhrase"
    class="c-input"
    placeholder="${userNumber + " above squirrel ..."}"
  ></textarea>
  <details
    data-id="phrase-warnings"
    class="c-card c-card--highlight is-hidden"
  >
    <summary>
      <span class="warnings-box-summary">Phrase may not be valid</span>
    </summary>
    <div id="warnings"></div>
  </details>
  <div class="c-button-group">
    <button id="inputSeedPhraseCancel" class="c-button c-button--secondary">
      Cancel
    </button>
    <button id="inputSeedPhraseContinue" class="c-button">Continue</button>
  </div>
`});

export const phraseRecoveryPage = async (
  userNumber: bigint,
  connection: Connection,
  device: DeviceData,
  prefilledPhrase?: string,
  message?: string
): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber, message), container);
  return init(userNumber, connection, device, prefilledPhrase);
};

const init = (
  userNumber: bigint,
  connection: Connection,
  device: DeviceData,
  prefilledPhrase?: string /* if set, prefilled as input */,
  message?: string
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
      "details[data-id=phrase-warnings]"
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
          warningsBox.classList.add("is-visible");
          warningsBox.classList.remove("is-hidden");

          // Actually generate the warnings
          for (const warning of warnings) {
            const div = document.createElement("div");
            render(mkWarningDiv(warning), div);
            warningsDiv.appendChild(div);
          }
        } else {
          warningsBox.classList.add("is-hidden");
          warningsBox.classList.remove("is-visible");
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
      resolve(cancel);
    };
    inputSeedPhraseContinue.onclick = async () => {
      const inputValue = inputSeedPhraseInput.value.trim();
      const mnemonic = dropLeadingUserNumber(inputValue);
      const result = apiResultToLoginFlowResult(
        await connection.fromSeedPhrase(userNumber, mnemonic, device)
      );

      switch (result.tag) {
        case "ok":
          resolve(result);
          break;
        case "err":
          await displayError({ ...result, primaryButton: "Try again" });
          phraseRecoveryPage(
            userNumber,
            connection,
            device,
            inputValue,
            message
          ).then((res) => resolve(res));
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
      return html`Recovering anchor
        <strong class="t-strong">${userNumber}</strong>, but recovery phrase
        suggests anchor <strong class="t-strong">${warning.anchor}</strong>`;
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
        <a
          target="_blank"
          class="t-link"
          href="/faq#${questions.invalidSeedphrase.anchor}"
          >${questions.invalidSeedphrase.question}</a
        >
      `;
    }
  }
};

const mkWarningDiv = (warningMessage: string | TemplateResult) => html` <div
  class="c-card c-card--highlight c-card--warning c-card--icon"
>
  <span class="c-card__icon">${warningIcon}</span>
  <div class="c-card__content">
    <p class="t-paragraph">${warningMessage}</p>
  </div>
</div>`;
