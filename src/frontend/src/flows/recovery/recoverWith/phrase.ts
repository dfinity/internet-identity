import { warningIcon } from "$src/components/icons";
import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import {
  dropLeadingUserNumber,
  RECOVERYPHRASE_WORDCOUNT,
} from "$src/crypto/mnemonic";
import type {
  LoginFlowCanceled,
  LoginFlowError,
  LoginFlowSuccess,
} from "$src/utils/flowResult";
import { apiResultToLoginFlowResult } from "$src/utils/flowResult";
import { DynamicKey } from "$src/utils/i18n";
import { Connection } from "$src/utils/iiConnection";
import { renderPage, withRef } from "$src/utils/lit-html";
import { RecoveryDevice } from "$src/utils/recoveryDevice";
import { Chan } from "$src/utils/utils";
import { isNullish } from "@dfinity/utils";
import { wordlists } from "bip39";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

const recoverWithPhraseTemplate = <
  /* The successful return type on verification */
  T extends { tag: "ok" },
  /* The error return type on verification */
  E extends { tag: "err"; message: string | DynamicKey }
>({
  confirm,
  verify,
  back,
  message,
}: {
  confirm: (result: T) => void;
  verify: (phrase: string) => Promise<T | E>;
  back: () => void;
  message: TemplateResult | string;
}) => {
  // All word input elements
  const numWords = RECOVERYPHRASE_WORDCOUNT + 1;
  const wordRefs: Ref<HTMLInputElement>[] = Array.from(
    { length: numWords },
    () => createRef()
  );

  // Read the phrase from the input elements
  const readPhrase = (): string | undefined => {
    const strings = [];
    for (const wordRef of wordRefs) {
      const value = withRef(wordRef, (input) => input.value);
      if (isNullish(value)) {
        return undefined;
      }

      strings.push(value);
    }

    return strings.join(" ");
  };

  // Read phrase from the page, verify it, and confirm back to the caller on success
  const verifyAndConfirm = async () => {
    const phrase = readPhrase();
    if (isNullish(phrase)) {
      toast.error("Could not read phrase");
      return;
    }
    const result = await verify(phrase);
    if (result.tag === "err") {
      toast.error("Could not use recovery phrase: " + result.message);
      console.warn(result);
      return;
    }

    confirm(result satisfies { tag: "ok" });
  };

  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">Enter recovery phrase</h1>
        <p class="t-lead">${message}</p>
      </hgroup>
      <div class="c-input c-input--recovery l-stack">
        <ol class="c-list c-list--recovery">
          ${wordRefs.map((wordRef, i) => wordTemplate({ wordRef, i }))}
        </ol>
      </div>

      <div class="c-button-group l-stack">
        <button
          @click=${() => back()}
          data-action="back"
          class="c-button c-button--secondary"
        >
          Back
        </button>
        <button
          @click=${() => verifyAndConfirm()}
          data-action="next"
          class="c-button"
        >
          Continue
        </button>
      </div>
    </article>
  `;

  return mainWindow({
    isWideContainer: true,
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

// Show a particular word
export const wordTemplate = ({
  wordRef,
  i,
}: {
  wordRef: Ref<HTMLInputElement>;
  i: number;
}): TemplateResult => {
  type State = "pending" | "incorrect";
  const state = new Chan<State>("pending");
  // Visual feedback depending on state
  const clazz = state.map(
    (s: State) =>
      ({
        pending: "c-list--recovery-word__attention",
        incorrect: "c-list--recovery-word__incorrect",
      }[s])
  );

  // The icon to show (when warning of a bad word)
  const icon = state.map(
    (s: State) =>
      ({
        pending: undefined,
        incorrect: html`<i class="c-list--recovery-word__icon"
          >${warningIcon}</i
        >`,
      }[s])
  );

  // Check if the word is ok (known bip39 word or anchor for the first input with index = 0)
  const isOk = (word: string): boolean => {
    // Avoid being annoying if user has not written anything but e.g. is just tabbing through the inputs
    if (word === "") {
      return true;
    }
    const isAnchorNumber = () => /\d+/.test(word);
    const isBipWord = wordlists.english.includes(word);

    return i === 0 ? isAnchorNumber() || isBipWord : isBipWord;
  };

  // Set the word as "incorrect"
  const setIncorrect = ({
    word,
    element,
  }: {
    word: string;
    element: HTMLInputElement;
  }) => {
    element.setCustomValidity(`Unknown word: '${word}'`);
    element.reportValidity();
    state.send("incorrect");
  };

  // Reset the "incorrect" warning
  const unsetIncorrect = ({ element }: { element: HTMLInputElement }) => {
    element.setCustomValidity("");
    element.reportValidity();
    state.send("pending");
  };

  // Helper to gain access to the event's target
  const withElement = <E extends Event>(
    event: E,
    f: (event: E, element: HTMLInputElement) => void
  ): void => {
    const element = event.currentTarget;
    if (!(element instanceof HTMLInputElement)) {
      return;
    }

    return f(event, element);
  };

  return html`<li
    style="--index: '${i + 1}'"
    class="c-list--recovery-word ${asyncReplace(clazz)}"
  >
    ${asyncReplace(icon)}
    <input
      @paste=${(e: ClipboardEvent) =>
        withElement(e, (event, element) => {
          e.preventDefault();

          // Get the text pasted
          if (event.clipboardData === null) {
            return;
          }
          const text = event.clipboardData.getData("text");

          // Split the text into words, dropping (leading) white spaces, empty strings (from e.g. double spaces), etc
          const [word = undefined, ...rest] = text
            .trimStart()
            .split(" ")
            .filter(Boolean);
          if (isNullish(word)) {
            return;
          }

          // Use the first word and set that as input value
          element.value = word;
          if (!isOk(word)) {
            setIncorrect({ word, element });
          }

          // Forward the rest of the text (if any) to the next input element (if any)
          if (rest.length <= 0) {
            return;
          }

          const newData = new DataTransfer();
          newData.setData("text", rest.join(" "));

          const newEvent = new ClipboardEvent("paste", {
            clipboardData: newData,
          });

          // Firefox does not set the clipboardData accuratly with the ClipboardEvent constructor. Empirical workaround.
          // Note: this cannot replace providing the clipboardData as initial value of the ClipboardEvent. Removing such value when using the contructor would not comply with specification and make the feature fails in Chrome and Safari.
          newEvent.clipboardData?.setData("text", rest.join(" "));

          // Go up until we find a list item, then to the next sibling, and finally back down until we find an input
          const next = element
            .closest("li")
            ?.nextElementSibling?.querySelector("input");
          if (isNullish(next)) {
            return;
          }

          next.dispatchEvent(newEvent);
        })}
      type="text"
      class="c-recoveryInput"
      ${ref(wordRef)}
      data-role="recovery-word-input"
      data-state=${asyncReplace(state)}
      @input=${(e: InputEvent) =>
        withElement(e, (_e, element) => {
          // Reset validity
          unsetIncorrect({ element });
        })}
      @change=${(e: InputEvent) =>
        withElement(e, (event, element) => {
          const word = element.value;
          if (!isOk(word)) {
            setIncorrect({ word, element });
          }
        })}
    />&nbsp;
  </li>`;
};

type TemplateProps<
  T extends { tag: "ok" },
  E extends { tag: "err"; message: string | DynamicKey }
> = Parameters<typeof recoverWithPhraseTemplate<T, E>>[0];

export const recoverWithPhrasePage = <
  T extends { tag: "ok" },
  E extends { tag: "err"; message: string | DynamicKey }
>(
  props: TemplateProps<T, E>
) => renderPage(recoverWithPhraseTemplate<T, E>)(props);

export const recoverWithPhrase = ({
  userNumber,
  connection,
  device,
  message,
}: {
  userNumber: bigint;
  connection: Connection;
  device: RecoveryDevice;
  message: TemplateResult | string;
}): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  return new Promise((resolve) => {
    recoverWithPhrasePage<LoginFlowSuccess, LoginFlowError>({
      confirm: (result) => resolve(result),
      verify: async (phrase: string) => {
        const mnemonic = dropLeadingUserNumber(phrase);
        const result = await withLoader(async () =>
          apiResultToLoginFlowResult(
            await connection.fromSeedPhrase(userNumber, mnemonic, device)
          )
        );
        return result;
      },
      back: () => resolve({ tag: "canceled" }),
      message,
    });
  });
};
