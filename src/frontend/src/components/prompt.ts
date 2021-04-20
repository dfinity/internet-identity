import { html, render } from "lit-html";
import { WebDialog } from "web-dialog";

const dialog = (question: string, secondaryMessage?: string) => html`<web-dialog
  id="prompt"
>
  <form action="" id="prompt-form">
    <p id="prompt-text">${question}</p>
    <p class="details">${secondaryMessage}</p>
    <input type="text" id="prompt-input" />
    <button type="button" id="prompt-cancel">Cancel</button>
    <button type="submit">Confirm</button>
  </form>
</web-dialog>`;

export const prompt = (
  question: string,
  secondaryMessage?: string
): Promise<string> => {
  const container = document.getElementById("notification") as HTMLElement;
  render(dialog(question, secondaryMessage), container);
  const promptDialog = document.querySelector("#prompt") as WebDialog;
  const promptForm = document.querySelector("#prompt-form") as HTMLFormElement;
  const promptInput = document.querySelector(
    "#prompt-input"
  ) as HTMLInputElement;
  const promptText = document.querySelector(
    "#prompt-text"
  ) as HTMLParagraphElement;
  const promptCancel = document.querySelector(
    "#prompt-cancel"
  ) as HTMLButtonElement;

  const closePrompt = (reject?: () => void) => {
    promptDialog.removeAttribute("open");
    promptForm.onsubmit = null;
    promptInput.value = "";
    reject?.();
  };

  return new Promise((resolve, reject) => {
    // setup text
    promptText.innerText = question;

    promptForm.onsubmit = (e) => {
      e.preventDefault;
      resolve(promptInput.value);
      closePrompt();
      return false;
    };

    promptCancel.onclick = () => closePrompt(reject);
    promptDialog.open = true;
  });
};
