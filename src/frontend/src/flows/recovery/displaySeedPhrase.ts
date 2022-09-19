import { html, render } from "lit";
import { checkmarkIcon, warningIcon, copyIcon } from "../../components/icons";

const pageContent = (seedPhrase: string) => html`
  <article class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--lead">Seedphrase</h1>
      <p class="t-lead">
        Your seed phrase makes it easy to recover this Identity Anchor.
      </p>
    </hgroup>
    <aside class="c-card c-card--icon c-card--warning" aria-label="Warning">
      <span class="c-card__icon">${warningIcon}</span>
      <p class="c-card__content">
        Do <b class="t-strong">NOT</b> forget to save this seed phrase. Save a
        backup on a storage medium and write it down.<br />
        Keep it secret &mdash; knowledge of the seed phrase will enable access
        to this Identity Anchor!
      </p>
    </aside>

    <h2 class="t-title">Your seed phrase</h2>
    <div>
      <output
        id="seedPhrase"
        data-seenPhrase
        translate="no"
        class="c-input c-input--readonly c-input--icon"
        >${seedPhrase}<i
          aria-label="Copy phrase to clipboard""
          title="Copy phrase to clipboard"
          id="seedCopy"
          class="c-button__icon c-input__icon"
          >${copyIcon}</i
        ></output
      >
    </div>

    <div style="margin: 2rem 0;">
      <input type="checkbox" id="my-checkbox" name="scales" />
      <label for="scales">I have stored my recovery phrase</label>
    </div>

    <button id="displaySeedPhraseContinue" class="c-button" disabled>
      Continue
    </button>
  </article>
`;

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

    const myCheckbox = document.getElementById(
      "my-checkbox"
    ) as HTMLInputElement;

    myCheckbox.onchange = () => {
      if (myCheckbox.checked) {
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
          const seedPhraseDiv = document.getElementById("seedPhrase");
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          selectText(seedPhraseDiv!);
          displaySeedPhraseContinue.classList.toggle("is-hidden", false);
          // render(checkmarkIcon, seedCopy); TODO do this or not?
        })
        .catch((e) => {
          console.error("Unable to copy seed phrase", e);
        });
    });
  });
