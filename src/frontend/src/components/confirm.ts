import { html, render } from "lit-html";
import { WebDialog } from "web-dialog";

const dialog = (question: string, secondaryMessage?: string) => html`<style>
    web-dialog,
    web-dialog * {
      background: var(--background-color);
      color: var(--text-color);
      --dialog-bg: var(--background-color);
    }
    #confirm-form {
      margin-bottom: 0.5rem;
    }
    .row button {
      margin-left: 0;
    }
    .details {
    }
  </style>
  <web-dialog id="confirm">
    <form action="" id="confirm-form">
      <section>
        <p id="confirm-text">${question}</p>
        <p class="details">${secondaryMessage}</p>
      </section>
      <section class="flex row">
        <button type="submit" class="primary">Confirm</button>
        <button type="button" id="confirm-cancel">Cancel</button>
      </section>
    </form>
  </web-dialog>`;

export const confirm = (
  question: string,
  secondaryMessage?: string
): Promise<boolean> => {
  const container = document.getElementById("notification") as HTMLElement;
  render(dialog(question, secondaryMessage), container);
  const confirmDialog = document.querySelector("#confirm") as WebDialog;
  const confirmForm = document.querySelector(
    "#confirm-form"
  ) as HTMLFormElement;
  const confirmText = document.querySelector(
    "#confirm-text"
  ) as HTMLParagraphElement;
  const confirmCancel = document.querySelector(
    "#confirm-cancel"
  ) as HTMLButtonElement;
  const details = confirmDialog?.querySelector(
    ".details"
  ) as HTMLParagraphElement;

  const closeConfirm = (reject?: (e: any) => void) => {
    confirmDialog.open = false;
    reject?.(false);
  };

  return new Promise((resolve, reject) => {
    // setup text
    confirmText.innerText = question;
    if (secondaryMessage) {
      details.innerText = secondaryMessage;
    }

    confirmForm.onsubmit = (e) => {
      e.preventDefault;
      resolve(true);
      closeConfirm();
      return false;
    };

    confirmCancel.onclick = () => closeConfirm(reject);
    confirmDialog.open = true;
  });
};
