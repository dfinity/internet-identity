import { warningIcon } from "$src/components/icons";
import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { RECOVERYPHRASE_WORDCOUNT } from "$src/crypto/mnemonic";
import type {
  LoginFlowCanceled,
  LoginFlowError,
  LoginFlowSuccess,
} from "$src/utils/flowResult";
import { apiResultToLoginFlowResult } from "$src/utils/flowResult";
import { DynamicKey } from "$src/utils/i18n";
import { Connection } from "$src/utils/iiConnection";
import { renderPage, withRef } from "$src/utils/lit-html";
import { parseUserNumber } from "$src/utils/userNumber";
import { Chan } from "$src/utils/utils";
import { isNullish, nonNullish } from "@dfinity/utils";
import { wordlists } from "bip39";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { ifDefined } from "lit-html/directives/if-defined.js";
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
  verify: (phrase: { userNumber: bigint; words: string[] }) => Promise<T | E>;
  back: () => void;
  message: TemplateResult | string;
}) => {
  // All word input elements
  const numWords = RECOVERYPHRASE_WORDCOUNT;
  const wordRefs: Ref<HTMLInputElement>[] = Array.from(
    { length: numWords },
    () => createRef()
  );

  const userNumberInput = createRef<HTMLInputElement>();

  // Read the phrase from the input elements
  const readPhrase = ():
    | { userNumber: bigint; words: string[] }
    | undefined => {
    const userNumberWord = withRef(userNumberInput, (input) => input.value);
    if (isNullish(userNumberWord) || userNumberWord === "") {
      return undefined;
    }

    const userNumber = parseUserNumber(userNumberWord);

    if (isNullish(userNumber)) {
      return undefined;
    }

    const words = [];
    for (const wordRef of wordRefs) {
      const value = withRef(wordRef, (input) => input.value);
      if (isNullish(value)) {
        return undefined;
      }

      words.push(value);
    }

    return { userNumber, words };
  };

  // Read phrase from the page, verify it, and confirm back to the caller on success
  const verifyAndConfirm = async () => {
    // Set (in)validity on the first word that hasn't been filled. We do only the first
    // one to not overwhelm the user.
    for (const input of [userNumberInput, ...wordRefs]) {
      if (input.value?.value === "") {
        input.value?.setCustomValidity("All fields should be filled");
        input.value?.reportValidity();
        break;
      }
    }

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
      <hgroup data-page="recover-with-phrase">
        <h1 class="t-title t-title--main">Input your Recovery Phrase</h1>
        <p class="t-lead">${message}</p>
      </hgroup>
      <div class="c-input c-input--recovery l-stack">
        <ol class="c-list c-list--recovery">
          ${wordTemplate({
            index: "#",
            classes: ["c-list--recovery-word--important"],
            assignTo: userNumberInput,
            placeholder: "Identity number",
            validityType: "number",
          })}
          ${wordRefs.map((wordRef, i) =>
            wordTemplate({
              assignTo: wordRef,
              index: `${i + 1}`,
              validityType: "word",
            })
          )}
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

type ValidityType = "word" | "number";

const reportValidity = ({ element }: { element: HTMLInputElement }): State => {
  const validityType = element.dataset.validityType;
  if (validityType !== "word" && validityType !== "number") {
    console.warn("Could not find validity for element");
    return "pending";
  }

  const word = element.value;
  const reason = {
    word: (word: string) =>
      word === ""
        ? "Word cannot be empty"
        : !wordlists.english.includes(word)
        ? "This is not a word that is associated with recovery phrases. Try again."
        : undefined,
    number: (word: string) =>
      word === ""
        ? "User number cannot be empty"
        : !/^\d+$/.test(word)
        ? "Enter your Internet Identity here. This is a number and contains no other characters."
        : undefined,
  }[validityType](word);
  if (nonNullish(reason)) {
    element.setCustomValidity(reason);
    element.reportValidity();
    return "incorrect";
  }

  return "pending";
};

const resetValidity = ({ element }: { element: HTMLInputElement }) => {
  element.setCustomValidity("");
  element.reportValidity();
};

type State = "pending" | "incorrect";

// Show a particular word
export const wordTemplate = ({
  assignTo,
  index,
  classes: classes_,
  validityType,
  placeholder,
}: {
  assignTo: Ref<HTMLInputElement>;
  index: string;
  classes?: string[];
  validityType: ValidityType;
  placeholder?: string;
}): TemplateResult => {
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
        // when the icon is clicked, we tell the browser to show the validity message again
        incorrect: html`<i
          @click=${() => withRef(assignTo, (input) => input.reportValidity())}
          class="c-list--recovery-word__icon"
          >${warningIcon}</i
        >`,
      }[s])
  );

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

  const classes = [...(classes_ ?? []), "c-list--recovery-word"];

  return html`<li
    style="--index: '${index}'"
    class="${classes.join(" ")} ${asyncReplace(clazz)}"
  >
    ${asyncReplace(icon)}
    <input
      autofocus
      data-validity-type=${validityType}
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
          // Trigger an event for the input to re-evaluate the validity
          element.dispatchEvent(new Event("change"));

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
      autocapitalize="none"
      spellcheck="false"
      class="c-recoveryInput"
      placeholder=${ifDefined(placeholder)}
      ${nonNullish(assignTo) ? ref(assignTo) : undefined}
      data-state=${asyncReplace(state)}
      @input=${(e: InputEvent) => {
        state.send("pending");
        withElement(e, (_e, element) => {
          // Reset validity when typing
          resetValidity({ element });
        });
      }}
      @change=${(e: InputEvent) => {
        withElement(e, (event, element) => {
          // Check validity when leaving the field
          state.send(reportValidity({ element }));
        });
      }}
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
  props: TemplateProps<T, E>,
  container?: HTMLElement
) => renderPage(recoverWithPhraseTemplate<T, E>)(props, container);

export const recoverWithPhrase = ({
  connection,
  message,
}: {
  connection: Connection;
  message: TemplateResult | string;
}): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  return new Promise((resolve) => {
    recoverWithPhrasePage<LoginFlowSuccess, LoginFlowError>({
      confirm: (result) => resolve(result),
      verify: async ({ userNumber, words }) => {
        const result = await withLoader(async () =>
          apiResultToLoginFlowResult(
            await connection.fromSeedPhrase(userNumber, words.join(" "))
          )
        );
        return result;
      },
      back: () => resolve({ tag: "canceled" }),
      message,
    });
  });
};
