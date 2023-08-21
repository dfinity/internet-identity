import { checkmarkIcon, copyIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { I18n } from "$src/i18n";
import { renderPage, withRef } from "$src/utils/lit-html";
import { html } from "lit-html";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";
import { phraseStepper } from "./stepper";

import copyJson from "./displaySeedPhrase.json";

const displaySeedPhraseTemplate = ({
  operation,
  userNumberWord,
  words,
  onContinue,
  cancel,
  copyPhrase: copyPhrase_,
  i18n,
}: {
  /* whether the phrase is created for the first time or just reset */
  operation: "create" | "reset";
  userNumberWord: string;
  words: string[];
  copyPhrase: () => Promise<void>;
  onContinue: () => void;
  cancel: () => void;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);

  const phraseCopyElement: Ref<HTMLElement> = createRef();

  const continueButton: Ref<HTMLButtonElement> = createRef();
  const checkbox: Ref<HTMLInputElement> = createRef();

  // Assume the phrase is a list of space-separated words
  const recoveryWords = words;

  // Copy the phrase and give visual feedback on success
  const copyPhrase = async () => {
    try {
      await copyPhrase_();
      withRef(phraseCopyElement, (phraseCopyElement) => {
        phraseCopyElement.classList.add("is-copied");
      });
    } catch (e: unknown) {
      toast.error(copy.unable_to_copy_phrase);
      console.error(copy.unable_to_copy_phrase, e);
    }
  };

  const pageContentSlot = html`
    <article>
      ${phraseStepper({ current: "store" })}
      <hgroup>
        <h1 class="t-title t-title--main">
          ${operation === "create" ? copy.title : copy.title_reset}
        </h1>
        <p class="t-lead">
          ${operation === "create" ? copy.header : copy.header_reset}
        </p>
      </hgroup>
      <div class="l-stack">
        <output class="c-output--recovery">
          <ol
            data-role="recovery-words"
            translate="no"
            class="c-list--recovery"
          >
            <li
              class="c-list--recovery-word c-list--recovery-word--important"
              style="--index: '#';"
            >
              ${userNumberWord}
            </li>
            ${recoveryWords.map(
              (word, i) =>
                html`<li
                  class="c-list--recovery-word"
                  style="--index: '${i + 1}';"
                >
                  <i>${word}</i>
                </li>`
            )}
          </ol>
          <i
            ${ref(phraseCopyElement)}
            @click=${() => copyPhrase()}
            aria-label=${copy.copy_to_clipboard}
            title=${copy.copy_to_clipboard}
            tabindex="0"
            id="seedCopy"
            class="c-button__icon"
          >
            <span>${copy.copy}</span>
            ${copyIcon} ${checkmarkIcon}
          </i></output
        >
      </div>

      <p class="t-paragraph l-stack">${copy.store_your_recovery_phrase}</p>

      <div class="l-stack">
        <input
          ${ref(checkbox)}
          type="checkbox"
          id="ack-checkbox"
          name="scales"
          @change=${() =>
            withRef(continueButton, (continueButton) =>
              withRef(checkbox, (checkbox) => {
                continueButton.disabled = !checkbox.checked;
              })
            )}
        />
        <label for="ack-checkbox" class="t-strong"
          >${copy.i_have_stored_phrase}</label
        >
      </div>
      <div class="c-button-group">
        <button
          @click=${() => cancel()}
          data-action="cancel"
          class="c-button c-button--secondary"
        >
          ${copy.cancel}
        </button>
        <button
          ${ref(continueButton)}
          @click=${() => onContinue()}
          id="displaySeedPhraseContinue"
          class="c-button"
          disabled
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

export const displaySeedPhrasePage = renderPage(displaySeedPhraseTemplate);

export const displaySeedPhrase = ({
  operation,
  seedPhrase,
}: {
  operation: "create" | "reset";
  seedPhrase: string;
}): Promise<"ok" | "canceled"> => {
  const i18n = new I18n();
  const words = seedPhrase.split(" ");
  // eslint-disable-next-line
  const userNumberWord = words.shift()!; // Extract first word (anchor) to show independently
  return new Promise((resolve) =>
    displaySeedPhrasePage({
      operation,
      userNumberWord,
      words,
      onContinue: () => resolve("ok"),
      cancel: () => resolve("canceled"),
      copyPhrase: () => navigator.clipboard.writeText(seedPhrase),
      i18n,
    })
  );
};
