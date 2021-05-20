import { html, render } from "lit-html";

const pageContent = (seedPhrase: string) => html`
  <div class="container">
    <h1>Your Seedphrase</h1>
    <textarea id="seedPhrase" readonly>
      ${seedPhrase}
    </textarea
    >
    <button id="copyButton">Copy</button>
    <button id="displaySeedPhraseContinue" class="primary">Continue</button>
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
    // TODO Setup copy button
  });
