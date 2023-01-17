import { html, render } from "lit-html";
import { checkmarkIcon, warningIcon, copyIcon } from "../../components/icons";
import { mainWindow } from "../../components/mainWindow";

const pageContent = (seedPhrase: string) => {
  const pageContentSlot = html`
    <article>
    <hgroup>
      <h1 class="t-title t-title--lead">Seedphrase</h1>
      <p class="t-lead">
        Your seed phrase makes it easy to recover this Identity Anchor.
      </p>
    </hgroup>
    <aside class="c-card c-card--icon c-card--warning l-stack" aria-label="Warning">
      <span class="c-card__icon">${warningIcon}</span>
      <p class="c-card__content">
        Do <b class="t-strong">NOT</b> forget to save this seed phrase. Save a
        backup on a storage medium and write it down.<br />
        Keep it secret &mdash; knowledge of the seed phrase will enable access
        to this Identity Anchor!
      </p>
    </aside>
    <h2 class="t-title l-stack">Your seed phrase</h2>
    <div>
      <output
        class="c-input c-input--textarea c-input--readonly c-input--icon"
        ><i translate="no" id="seedPhrase">${seedPhrase}</i><i
          aria-label="Copy phrase to clipboard""
          title="Copy phrase to clipboard"
          tabindex="0"
          id="seedCopy"
          class="c-button__icon c-input__icon"
          >
            <span>copy to clipboard</span>
            ${copyIcon}
            ${checkmarkIcon}
          </i
        ></output
      >
    </div>

    <div style="margin: 2rem 0;">
      <input type="checkbox" id="ack-checkbox" name="scales" />
      <label for="ack-checkbox">I have stored my seed phrase</label>
    </div>

    <button id="displaySeedPhraseContinue" class="c-button" disabled>
      Continue
    </button>
  </article>
`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const displaySeedPhrase = async (seedPhrase: string): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(seedPhrase), container);
  return init();
};

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displaySeedPhraseContinue = document.getElementById(
      "displaySeedPhraseContinue"
    ) as HTMLButtonElement;
    displaySeedPhraseContinue.onclick = () => resolve();

    const checkbox = document.getElementById(
      "ack-checkbox"
    ) as HTMLInputElement;

    checkbox.onchange = () => {
      if (checkbox.checked) {
        displaySeedPhraseContinue.disabled = false;
      } else {
        displaySeedPhraseContinue.disabled = true;
      }
    };

    const seedCopy = document.getElementById("seedCopy") as HTMLButtonElement;
    const seedPhrase = document.getElementById("seedPhrase")
      ?.innerText as string;

    const selectText = (element: HTMLElement) => {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const selection = window.getSelection()!;
      const range = document.createRange();
      range.selectNodeContents(element);
      selection.removeAllRanges();
      selection.addRange(range);
    };

    seedCopy.addEventListener("click", () => {
      navigator.clipboard
        .writeText(seedPhrase)
        .then(() => {
          const seedPhraseElem = document.getElementById("seedPhrase");
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          selectText(seedPhraseElem!);
          displaySeedPhraseContinue.classList.toggle("is-hidden", false);
          seedCopy.classList.add("is-copied");
        })
        .catch((e) => {
          console.error("Unable to copy seed phrase", e);
        });
    });
  });
