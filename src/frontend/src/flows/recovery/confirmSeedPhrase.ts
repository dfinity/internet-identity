import { mkAnchorInput } from "$src/components/anchorInput";
import { checkmarkIcon, warningIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { renderPage, withRef } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

import copyJson from "./confirmSeedPhrase.json";

// A list of words, where "check" indicates if the user needs to double check (re-input) a word
type Word = { word: string } & (
  | { check: false }
  | { check: true; elem: Ref<HTMLInputElement>; shouldFocus: boolean }
);

// A list of indices nicely spread over the 24 BIP 39 words (anchor + 24 BIP39)
export const checkIndices = [0, 1, 23];

// Check that a word has been input correctly
const checkWord = (word: Word): boolean =>
  // If the word is not one that needs checking, then return
  !word.check ||
  // otherwise, make sure the input's value matches the HTML data-expected attribute
  withRef(word.elem, (elem) => elem.value === elem.dataset.expected) === true;

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
      // NOTE: typescript can't follow if word is destructured with {...word}
      return { word: word.word, check: word.check, elem, shouldFocus: false };
    } else {
      return { word: word.word, check: word.check };
    }
  });

  // Focus the first word (that needs to be checked)
  for (const word of words) {
    if (word.check) {
      word.shouldFocus = true;
      break;
    }
  }

  // if the identity number has been re-input correctly
  const identityOk = new Chan<"pending" | "wrong" | "correct">("pending");
  // if all "check" words have been re-input correctly
  const wordsOk = new Chan<boolean>(false);
  // if the confirmation button is disabled
  const zipped = identityOk.zip(wordsOk);
  const disabled = zipped.map(
    ([idOk, wdsOk]) => !(idOk === "correct" && wdsOk)
  );

  // TODO: test this
  const anchorInput = mkAnchorInput({
    onSubmit: (userNumber: bigint) => {
      const actual = BigInt(userNumber);
      const expected = BigInt(userNumberWord);
      if (actual === expected) {
        identityOk.send("correct");
      } else {
        identityOk.send("wrong");
      }
    },
    onInput: (a) => {
      const actual = BigInt(a);
      const expected = BigInt(userNumberWord);
      if (actual === expected) {
        identityOk.send("correct");
      } else {
        identityOk.send("pending");
      }
    },
    onChange: (a) => {
      const actual = BigInt(a);
      const expected = BigInt(userNumberWord);

      if (actual === expected) {
        identityOk.send("correct");
      } else {
        identityOk.send("wrong");
      }
    },
    classes: identityOk.map(
      (state) =>
        ({
          pending: [],
          correct: ["c-input--anchor__wrap--good"],
          wrong: ["c-input--anchor__wrap--error"],
        }[state])
    ),
    dataExpected: userNumberWord,
  });

  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">${copy.title}</h1>
        <p class="t-lead">${copy.header}</p>
      </hgroup>
      ${anchorInput.template}
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
      style="--index: '${i + 1}'"
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
    style="--index: '${i + 1}'"
    class="c-list--recovery-word ${asyncReplace(clazz)}"
  >
    ${asyncReplace(icon)}
    <input
      type="text"
      class="c-recoveryInput"
      ${ref(word.elem)}
      data-expected=${word.word}
      ?autofocus=${word.shouldFocus}
      data-state=${asyncReplace(state)}
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
