import { html } from "lit-html";
import { withRef, renderPage } from "../../utils/lit-html";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import { checkmarkIcon, copyIcon } from "../../components/icons";
import { mainWindow } from "../../components/mainWindow";
import { toast } from "../../components/toast";

const displaySeedPhraseTemplate = ({
  seedPhrase,
  onContinue,
  copyPhrase: copyPhrase_,
}: {
  seedPhrase: string;
  copyPhrase: () => Promise<void>;
  onContinue: () => void;
}) => {
  const phraseElement: Ref<HTMLElement> = createRef();
  const phraseCopyElement: Ref<HTMLElement> = createRef();

  const continueButton: Ref<HTMLButtonElement> = createRef();
  const checkbox: Ref<HTMLInputElement> = createRef();

  // Selects the phrase content to give visual feedback of what's been copied
  const selectPhrase = () => {
    withRef(phraseElement, (phraseElement) => {
      const selection = window.getSelection();
      if (selection !== null) {
        const range = document.createRange();
        range.selectNodeContents(phraseElement);
        selection.removeAllRanges();
        selection.addRange(range);
      }
    });
  };

  // Copy the phrase and give visual feedback on success
  const copyPhrase = async () => {
    try {
      await copyPhrase_();
      selectPhrase();
      withRef(phraseCopyElement, (phraseCopyElement) => {
        phraseCopyElement.classList.add("is-copied");
      });
    } catch (e: unknown) {
      toast.error("Unable to copy seed phrase");
      console.error("Unable to copy seed phrase", e);
    }
  };

  const pageContentSlot = html`
    <article>
    <hgroup>
      <h1 class="t-title t-title--main">Store Recovery Phrase</h1>
      <p class="t-lead">
         If you lose access to your devices, use your recovery phrase to access your Internet Identity.
      </p>
    </hgroup>
    <h2 class="t-title l-stack">Your recovery phrase</h2>
    <div>
      <output
        class="c-input c-input--textarea c-input--readonly c-input--icon"
        ><i translate="no"
        ${ref(phraseElement)}
        id="seedPhrase">${seedPhrase}</i><i
            ${ref(phraseCopyElement)}
            @click=${() => copyPhrase()}
          aria-label="Copy phrase to clipboard""
          title="Copy phrase to clipboard"
          tabindex="0"
          id="seedCopy"
          class="c-button__icon c-input__icon"
          >
            <span>Copy</span>
            ${copyIcon}
            ${checkmarkIcon}
          </i
        ></output
      >
    </div>

    <p class="t-paragraph">
      Securely store your recovery phrase, and do not share it with anyone!
    </p>

    <div class="l-stack">
      <input ${ref(checkbox)} type="checkbox" id="ack-checkbox" name="scales"
        @change=${() =>
          withRef(continueButton, (continueButton) =>
            withRef(checkbox, (checkbox) => {
              continueButton.disabled = !checkbox.checked;
            })
          )}
      />
      <label for="ack-checkbox" class="t-strong">I have stored my recovery phrase.</label>
    </div>
    <div class="l-stack">
      <button ${ref(continueButton)}
      @click=${() => onContinue()}
      id="displaySeedPhraseContinue" class="c-button" disabled>
        Continue
      </button>
    </div>
  </article>
`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const displaySeedPhrasePage = renderPage(displaySeedPhraseTemplate);

export const displaySeedPhrase = (seedPhrase: string): Promise<void> => {
  return new Promise((resolve) =>
    displaySeedPhrasePage({
      seedPhrase,
      onContinue: () => resolve(),
      copyPhrase: () => navigator.clipboard.writeText(seedPhrase),
    })
  );
};
