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
import { mainWindow } from "../../../components/mainWindow";
import { warnBox } from "../../../components/warnBox";

const pageContent = (userNumber: bigint, message?: string) => {
  const pageContentSlot = html`
    <hgroup>
      <h1 class="t-title t-title--main">Enter recovery phrase</h1>
      <p class="t-lead">
        ${message !== undefined
          ? message
          : "Type your recovery phrase below to access your Internet Identity."}
      </p>
    </hgroup>
    <textarea
      id="inputSeedPhrase"
      class="c-input"
      placeholder="${userNumber + " above squirrel ..."}"
    ></textarea>
    <div data-id="phrase-warnings" class="is-hidden">
      ${warnBox({
        title: "Phrase may not be valid",
        message: html`<ul
          class="c-list c-list--bulleted l-stack"
          id="warnings"
        ></ul>`,
        htmlElement: "div",
        additionalClasses: ["l-stack"],
      })}
    </div>
    <div class="c-button-group">
      <button id="inputSeedPhraseCancel" class="c-button c-button--secondary">
        Cancel
      </button>
      <button id="inputSeedPhraseContinue" class="c-button">Continue</button>
    </div>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

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
      "[data-id=phrase-warnings]"
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
            const li = document.createElement("li");
            li.classList.add("c-list__item");
            render(html`${warning}`, li);
            warningsDiv.appendChild(li);
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
      const faqEntry =
        "https://support.dfinity.org/hc/en-us/articles/12542473193236-Why-do-I-get-Invalid-Recovery-Phrase-when-I-try-to-recover-my-Identity-Anchor-";
      return html`
        This does not look like a seed phrase generated by Internet Identity,
        please make sure to copy the full seedphrase and try again. For more
        information, please see
        <a
          target="_blank"
          class="t-link"
          rel="noopener noreferrer"
          href=${faqEntry}
          >the FAQ</a
        >.
      `;
    }
  }
};
