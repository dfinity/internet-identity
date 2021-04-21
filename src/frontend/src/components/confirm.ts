import { html, render } from "lit-html";
import { WebDialog } from "web-dialog";

type ConfirmOptions = {
  message: string;
  detail?: string;
  yesText?: string;
  noText?: string;
};
const dialog = (options: ConfirmOptions) => html`<style>
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
    section {
      width: calc(var(--dialog-width) - (2 * var(--dialog-padding)));
    }
    p {
      margin: 0 0 1rem;
    }
  </style>
  <web-dialog id="confirm">
    <form action="" id="confirm-form">
      <section>
        <p id="confirm-text">${options.message}</p>
        <p class="details">${options.detail}</p>
      </section>
      <section class="flex row">
        <button type="submit" class="primary">
          ${options.yesText ?? "Confirm"}
        </button>
        <button type="button" id="confirm-cancel">
          ${options.noText ?? "Cancel"}
        </button>
      </section>
    </form>
  </web-dialog>`;

export const confirm = (options: ConfirmOptions): Promise<boolean> => {
  const container = document.getElementById("notification") as HTMLElement;
  render(dialog(options), container);
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
  if (details.innerText.trim() === "show more") {
    details.remove();
  }
  const closeConfirm = (reject?: (e: any) => void) => {
    confirmDialog.open = false;
    reject?.(false);
  };

  return new Promise((resolve, reject) => {
    // setup text
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
