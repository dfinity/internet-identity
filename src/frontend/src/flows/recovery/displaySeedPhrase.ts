import { html } from "lit-html";
import { withRef, renderPage } from "../../utils/lit-html";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import { checkmarkIcon, copyIcon } from "../../components/icons";
import { I18n } from "../../i18n";
import { mainWindow } from "../../components/mainWindow";
import { toast } from "../../components/toast";

import copyJson from "./displaySeedPhrase.json";

const displaySeedPhraseTemplate = ({
  seedPhrase,
  onContinue,
  copyPhrase: copyPhrase_,
  i18n,
}: {
  seedPhrase: string;
  copyPhrase: () => Promise<void>;
  onContinue: () => void;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);
  const staticCopy = i18n.staticLang(copyJson);

  const phraseCopyElement: Ref<HTMLElement> = createRef();

  const continueButton: Ref<HTMLButtonElement> = createRef();
  const checkbox: Ref<HTMLInputElement> = createRef();

  // Assume the phrase is a list of space-separated words
  const recoveryWords = seedPhrase.split(" ");

  // Copy the phrase and give visual feedback on success
  const copyPhrase = async () => {
    try {
      await copyPhrase_();
      withRef(phraseCopyElement, (phraseCopyElement) => {
        phraseCopyElement.classList.add("is-copied");
      });
    } catch (e: unknown) {
      toast.error(staticCopy.unable_to_copy_phrase);
      console.error(staticCopy.unable_to_copy_phrase, e);
    }
  };

  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">${copy.title}</h1>
        <p class="t-lead">${copy.header}</p>
      </hgroup>
      <h2 class="t-title l-stack">${copy.your_recovery_phrase}</h2>
      <div>
        <output
          class="c-input c-input--textarea c-input--textarea-narrow c-input--readonly c-input--icon"
          ><ol translate="no" class="c-list c-list--recovery">
            ${recoveryWords.map(
              (word, i) =>
                html`<li style="--i: ${i / recoveryWords.length}">${word}</li>`
            )}
          </ol>
          <i
            ${ref(phraseCopyElement)}
            @click=${() => copyPhrase()}
            aria-label=${copy.copy_to_clipboard}
            title=${copy.copy_to_clipboard}
            tabindex="0"
            id="seedCopy"
            class="c-button__icon c-input__icon"
          >
            <span>${copy.copy}</span>
            ${copyIcon} ${checkmarkIcon}
          </i></output
        >
      </div>

      <p class="t-paragraph">${copy.store_your_recovery_phrase}</p>

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
      <div class="l-stack">
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

export const displaySeedPhrase = (seedPhrase: string): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    displaySeedPhrasePage({
      seedPhrase,
      onContinue: () => resolve(),
      copyPhrase: () => navigator.clipboard.writeText(seedPhrase),
      i18n,
    })
  );
};
