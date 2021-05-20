import ClipboardJS from "clipboard";
import { html, render } from "lit-html";
import { checkmarkIcon } from "../../components/icons";

const pageContent = (seedPhrase: string) => html`
  <div class="container">
    <h1>Your Seedphrase</h1>
    <textarea id="seedPhrase" readonly>
      ${seedPhrase}
    </textarea
    >
    <button id="seedCopy" data-clipboard-target="#seedPhrase">Copy</button>
    <button id="displaySeedPhraseContinue" class="primary">Continue</button>
  </div>
`;

export const displaySeedPhrase = async (seedPhrase: string): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(seedPhrase.trim()), container);
  return init();
};

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displaySeedPhraseContinue = document.getElementById(
      "displaySeedPhraseContinue"
    ) as HTMLButtonElement;
    displaySeedPhraseContinue.onclick = () => resolve();

    const seedCopy = document.getElementById("seedCopy") as HTMLButtonElement;
    new ClipboardJS(seedCopy).on("success", () => {
      const seedCopy = document.getElementById("seedCopy") as HTMLButtonElement;
      render(checkmarkIcon, seedCopy);
    });
  });
