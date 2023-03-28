import { html, TemplateResult, render } from "lit-html";
import { withRef } from "../utils/lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

/**
 * creates a modal that contains arbitrary content
 **/
export const createModal = ({
  slot,
  closeButtonRef,
  submitButtonRef,
}: {
  slot: TemplateResult;
  closeButtonRef: Ref<HTMLButtonElement>;
  submitButtonRef: Ref<HTMLButtonElement>;
}): HTMLDialogElement => {
  const modalHtml = html`
    <form method="dialog" class="c-modal__content c-card c-card--modal">
      <div class="c-modal__inner">
        <button
          class="c-modal__close"
          aria-label="Close Modal"
          ${ref(closeButtonRef)}
        >
          &times;
        </button>
        ${slot}
        <div class="c-modal__footer">
          <button
            type="submit"
            class="c-button c-button--primary"
            ${ref(submitButtonRef)}
          >
            OK
          </button>
        </div>
      </div>
    </form>
  `;

  const modalElement = document.createElement("dialog");
  modalElement.classList.add("c-modal");
  modalElement.setAttribute("aria-modal", "true");

  render(modalHtml, modalElement);

  return modalElement;
};

export const modal = ({ slot }: { slot: TemplateResult }): Promise<void> => {
  const closeButtonRef: Ref<HTMLButtonElement> = createRef();
  const submitButtonRef: Ref<HTMLButtonElement> = createRef();

  const modalElement = createModal({
    slot,
    closeButtonRef,
    submitButtonRef,
  });

  document.body.appendChild(modalElement);

  return new Promise((resolve, reject) => {
    // open modal using the browsers API
    modalElement.showModal();

    const close = () => {
      modalElement.close();
      modalElement.remove();
    };

    withRef(closeButtonRef, (closeButton) => {
      closeButton.onclick = () => {
        reject();
        close();
      };
    });
    withRef(submitButtonRef, (submitButton) => {
      submitButton.onclick = () => {
        resolve();
        close();
      };
    });
  });
};
