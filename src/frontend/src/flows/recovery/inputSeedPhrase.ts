import { html, render } from "lit-html";
import { displayError } from "../../components/displayError";
import { validate } from "../../crypto/mnemonic";
import { parseUserNumber } from "../../utils/userNumber";

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
    const inputSeedPhraseInput = document.getElementById(
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
    inputSeedPhraseContinue.onclick = async () => {
      const inputValue = dropLeadingUserNumber(
        inputSeedPhraseInput.value.trim()
      );
      if (validate(inputValue)) {
        resolve(inputValue);
      } else {
        await displayError({
          title: "Invalid Seedphrase",
          message:
            "This is not a seed phrase generated by Internet Identity, please make sure to copy the full seedphrase and try again.",
          primaryButton: "Try again",
        });
        resolve(inputSeedPhrase());
      }
    };
  });

const dropLeadingUserNumber = (s: string): string => {
  const i = s.indexOf(" ");
  if (i !== -1 && parseUserNumber(s.slice(0, i)) !== null) {
    return s.slice(i + 1);
  } else {
    return s;
  }
};
