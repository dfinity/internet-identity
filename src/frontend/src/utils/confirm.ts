import { WebDialog } from "web-dialog";

export const confirm = (
  question: string,
  secondaryMessage?: string
): Promise<boolean> => {
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
  const details = confirmDialog.querySelector(
    ".details"
  ) as HTMLParagraphElement;

  const closeConfirm = (reject?: (e: any) => void) => {
    confirmDialog.removeAttribute("open");
    confirmText.innerText = "";
    details.innerText = "";
    confirmForm.onsubmit = null;
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
