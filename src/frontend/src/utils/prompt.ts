import { WebDialog } from "web-dialog";

export const prompt = (question): Promise<string> => {
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
