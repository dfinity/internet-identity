import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { withRef, renderPage } from "../../utils/lit-html";
import { Chan } from "../../utils/utils";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";
import { warningIcon, checkmarkIcon } from "../../components/icons";

import copyJson from "./confirmSeedPhrase.json";

// A list of words, where "check" indicates if the user needs to double check (re-input) a word
type Word = { word: string } & (
  | { check: false }
  | { check: true; elem: Ref<HTMLInputElement>; shouldFocus?: boolean }
);

// A list of indices nicely spread over the 25 words (anchor + 24 BIP39)
export const checkIndices = [3, 7, 12, 13, 15, 17, 18, 20, 22, 24];

// Check that a word has been input correctly
const checkWord = (word: Word): boolean =>
  // If the word is not one that needs checking, then return
  !word.check ||
  // otherwise, make sure the input's value matches the HTML data-expected attribute
  withRef(word.elem, (elem) => elem.value === elem.dataset.expected) === true;

const confirmSeedPhraseTemplate = ({
  words: words_,
  confirm,
  back,
  i18n,
}: {
  words: Omit<Word, "elem">[];
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

  // find the first editable word and focus it
  const firstEditable = words.find((w) => w.check);
  if (firstEditable && "elem" in firstEditable) {
    firstEditable.shouldFocus = true;
  }

  // if all "check" words have been re-input correctly
  const wordsOk = new Chan<boolean>(false);
  // if the confirmation button is disabled
  const disabled = wordsOk.map((ok) => !ok);

  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">${copy.title}</h1>
        <p class="t-lead">${copy.header}</p>
      </hgroup>
      <div class="c-input c-input--recovery l-stack">
        <ol class="c-list c-list--recovery">
          ${words.map((word, i) =>
            wordTemplate({
              word,
              /* on word update, re-check all words */
              update: () => wordsOk.send(words.every(checkWord)),
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
      style="--index: '${i}'"
      class="c-list--recovery-word c-list--recovery-word__disabled"
    >
      ${word.word}
    </li>`;
  }

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

  return html`<li
    style="--index: '${i}'"
    class="c-list--recovery-word ${asyncReplace(clazz)}"
  >
    ${asyncReplace(icon)}
    <input
      type="text"
      class="c-recoveryInput"
      ${ref(word.elem)}
      data-expected=${word.word}
      ?autofocus=${word.shouldFocus}
      data-state=${asyncReplace(
        state.map(
          (x) => x
        ) /* workaround because chan supports only one .recv() */
      )}
      @input=${() => {
        /* On input, immediately show word as correct when correct, but don't show if a
         * word is incorrect (done only when leaving the field) to not freak out user as they type */
        state.send(checkWord(word) ? "correct" : "pending");
        update();
      }}
      @change=${() => {
        /* When leaving the field show if the word is corrrect or not */
        state.send(checkWord(word) ? "correct" : "incorrect");
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
    confirmSeedPhrasePage({
      words: phrase.split(" ").map((word, i) => ({
        word,
        check: checkIndices.includes(i),
      })),
      confirm: () => resolve("confirmed"),
      back: () => resolve("back"),
      i18n,
    });
  });
};
