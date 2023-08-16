import { checkmarkIcon, warningIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { renderPage, withRef } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";
import { phraseStepper } from "./stepper";

import copyJson from "./confirmSeedPhrase.json";

// A list of words, where "check" indicates if the user needs to double check (re-input) a word
type Word = { word: string } & (
  | { check: false }
  | { check: true; elem: Ref<HTMLInputElement> }
);

// A list of indices nicely spread over the 24 BIP39 words (anchor is separate)
export const checkIndices = [0, 1, 23];

// Check that a word has been input correctly
const checkExpected = (elem: Ref<HTMLInputElement>): boolean =>
  // make sure the input's value matches the HTML data-expected attribute
  withRef(elem, (elem) => elem.value === elem.dataset.expected) === true;

const confirmSeedPhraseTemplate = ({
  userNumberWord,
  words: words_,
  confirm,
  back,
  i18n,
}: {
  userNumberWord: string;
  words: Pick<Word, "word" | "check">[];
  confirm: () => void;
  back: () => void;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);

  // All words, where a `Ref` was added if the word needs checking
  const words: Word[] = words_.map((word) => {
    if (word.check) {
      const elem: Ref<HTMLInputElement> = createRef();
      // NOTE: typescript can't follow if word is deconstructed with {...word}
      return { word: word.word, check: word.check, elem };
    } else {
      return { word: word.word, check: word.check };
    }
  });

  // if all "check" words have been re-input correctly
  const wordsOk = new Chan<boolean>(false);
  // if the number has been re-input correctly
  const numberOk = new Chan<boolean>(false);
  // if the confirmation button is disabled
  const disabled = numberOk
    .zip(wordsOk)
    .map(([nbOk, wdsOk]) => !(nbOk && wdsOk));

  const userNumberInputRef = createRef<HTMLInputElement>();
  const userNumberInput = nudgeWord({
    update: () => numberOk.send(checkExpected(userNumberInputRef)),
    expected: userNumberWord,
    assignTo: userNumberInputRef,
    index: "#",
    classes: ["c-list--recovery-word--important"],
    placeholder: "Identity number",
    mismatchMessage: "This does not match your identity number.",
  });

  const pageContentSlot = html`
    <article>
      ${phraseStepper({ current: "confirm" })}
      <hgroup>
        <h1 class="t-title t-title--main">${copy.title}</h1>
        <p class="t-lead">${copy.header}</p>
      </hgroup>
      <div class="c-input c-input--recovery l-stack">
        <ol class="c-list c-list--recovery">
          ${userNumberInput}
          ${words.map((word, i) =>
            wordTemplate({
              word,
              /* on word update, re-check all words */
              update: () =>
                wordsOk.send(
                  words.every(
                    (word) =>
                      // If the word is not one that needs checking, then ok
                      !word.check || checkExpected(word.elem)
                  )
                ),
              i,
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
          ${copy.back}
        </button>
        <button
          @click=${() => confirm()}
          data-action="next"
          class="c-button"
          ?disabled=${asyncReplace(disabled)}
        >
          ${copy.continue}
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
  word,
  update,
  i,
}: {
  word: Word;
  /* Notify the caller that a word was updated */
  update: () => void;
  i: number;
}): TemplateResult => {
  // In the simple case the word doesn't need checking and is simply displayed
  if (!word.check) {
    return html`<li
      style="--index: '${i + 1}'"
      class="c-list--recovery-word c-list--recovery-word__disabled"
    >
      ${word.word}
    </li>`;
  }

  return nudgeWord({
    update,
    expected: word.word,
    index: `${i + 1}`,
    assignTo: word.elem,
    mismatchMessage: `This does not match the expected word (word ${
      i + 1
    } in your recovery phrase).`,
  });
};

const nudgeWord = ({
  update,
  expected,
  index,
  assignTo,
  classes: classes_,
  placeholder,
  mismatchMessage,
}: {
  update: () => void;
  expected: string;
  index: string;
  assignTo: Ref<HTMLInputElement>;
  classes?: string[];
  placeholder?: string;
  mismatchMessage: string;
}): TemplateResult => {
  type State = "pending" | "correct" | "incorrect";
  const state = new Chan<State>("pending");
  // Visual feedback depending on state
  const clazz = state.map(
    (s: State) =>
      ({
        pending: "c-list--recovery-word__attention",
        correct: "c-list--recovery-word__correct",
        incorrect: "c-list--recovery-word__incorrect",
      }[s])
  );

  const icon = state.map(
    (s: State) =>
      ({
        pending: undefined,
        correct: html`<i class="c-list--recovery-word__icon"
          >${checkmarkIcon}</i
        >`,
        incorrect: html`<i class="c-list--recovery-word__icon"
          >${warningIcon}</i
        >`,
      }[s])
  );

  const classes = [...(classes_ ?? []), "c-list--recovery-word"];

  return html`<li
    style="--index: '${index}'"
    class="${classes.join(" ")} ${asyncReplace(clazz)}"
    @click=${() => withRef(assignTo, (input) => input.reportValidity())}
  >
    ${asyncReplace(icon)}
    <input
      autofocus
      type="text"
      autocapitalize="none"
      spellcheck="false"
      class="c-recoveryInput"
      ${ref(assignTo)}
      data-expected=${expected}
      data-state=${asyncReplace(state)}
      placeholder=${ifDefined(placeholder)}
      @input=${() => {
        /* On input, immediately show word as correct when correct, but don't show if a
         * word is incorrect (done only when leaving the field) to not freak out user as they type */
        const state_ = checkExpected(assignTo) ? "correct" : "pending";
        assignTo.value?.setCustomValidity("");
        assignTo.value?.reportValidity();
        state.send(state_);
        update();
      }}
      @change=${() => {
        const state_ = checkExpected(assignTo) ? "correct" : "incorrect";
        assignTo.value?.setCustomValidity(
          state_ === "correct" ? "" : mismatchMessage
        );
        assignTo.value?.reportValidity();
        state.send(state_);
        update();
      }}
    />&nbsp;
  </li>`;
};

export const confirmSeedPhrasePage = renderPage(confirmSeedPhraseTemplate);

export const confirmSeedPhrase = ({
  phrase,
}: {
  phrase: string;
}): Promise<"confirmed" | "back"> => {
  return new Promise((resolve) => {
    const i18n = new I18n();
    const words = phrase.split(" ");
    // eslint-disable-next-line
    const userNumberWord = words.shift()!; // Extract first word (anchor) to show independently
    confirmSeedPhrasePage({
      userNumberWord,
      words: words.map((word, i) => ({
        word,
        check: checkIndices.includes(i),
      })),
      confirm: () => resolve("confirmed"),
      back: () => resolve("back"),
      i18n,
    });
  });
};
