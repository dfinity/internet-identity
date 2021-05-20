import { html, render } from "lit-html";

const pageContent = () => html`
  <style>
    #inputSeedPhrase {
      width: 100%;
      height: 6rem;
      box-sizing: border-box;
      margin-bottom: 1rem;
      font-size: 1rem;
      font-weight: 400;
    }
  </style>
  <div class="container">
    <h1>Your seed phrase</h1>
    <p>Please provide your seed phrase</p>
    <textarea id="inputSeedPhrase" placeholder="Your seed phrase"></textarea>
    <button id="inputSeedPhraseContinue" class="primary">Continue</button>
    <button id="inputSeedPhraseCancel">Cancel</button>
  </div>
`;

export const inputSeedPhrase = async (): Promise<string | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init();
};

const init = (): Promise<string | null> =>
  new Promise((resolve) => {
    const inputSeedPhrase = document.getElementById(
      "inputSeedPhrase"
    ) as HTMLInputElement;
    const inputSeedPhraseContinue = document.getElementById(
      "inputSeedPhraseContinue"
    ) as HTMLButtonElement;
    const inputSeedPhraseCancel = document.getElementById(
      "inputSeedPhraseCancel"
    ) as HTMLButtonElement;
    inputSeedPhraseCancel.onclick = () => {
      resolve(null);
    };
    inputSeedPhraseContinue.onclick = () => {
      resolve(inputSeedPhrase.value.trim());
    };
  });
