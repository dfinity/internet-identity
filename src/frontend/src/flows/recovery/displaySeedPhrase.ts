import { html, render } from "lit-html";
import { checkmarkIcon, warningIcon } from "../../components/icons";

const pageContent = (seedPhrase: string) => html`
  <style>
    #seedPhrase {
      font-size: 1rem;
    }
  </style>
  <div class="container">
    <h1>Seedphrase</h1>
    <p>Your seed phrase makes it easy to recover this Identity Anchor.</p>
    <div class="warningBox">
      <span class="warningIcon">${warningIcon}</span>
      <div class="warningMessage">
        Do <b>NOT</b> forget to save this seed phrase. Save a backup on a
        storage medium and write it down.<br />
        Keep it secret &mdash; knowledge of the seed phrase will enable access
        to this Identity Anchor!
      </div>
    </div>
    <label>Your seed phrase</label>
    <div id="seedPhrase" translate="no" class="highlightBox">${seedPhrase}</div>
    <button id="seedCopy" data-clipboard-target="#seedPhrase">Copy</button>
    <button id="displaySeedPhraseContinue" class="primary hidden">
      Continue
    </button>
  </div>
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
          displaySeedPhraseContinue.classList.toggle("hidden", false);
          render(checkmarkIcon, seedCopy);
          seedCopy.title = "copied";
        })
        .catch((e) => {
          console.error("Unable to copy seed phrase", e);
        });
    });
  });
