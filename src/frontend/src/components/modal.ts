import { html, TemplateResult, render } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

/**
 * creates a modal that contains arbitrary content
 * and ID is obligatory for accessibility reasons
 * use crypto.randomUUID() if you feel uninspired
 **/
export const createModal = ({
  slot,
  id,
  closeButtonRef,
  submitButtonRef,
}: {
  slot: TemplateResult;
  id: string;
  closeButtonRef: Ref<HTMLButtonElement>;
  submitButtonRef: Ref<HTMLButtonElement>;
}): HTMLDialogElement => {
  const modalHtml = html`
    <form method="dialog" class="c-modal__content c-card c-card--modal">
      <div class="c-modal__inner">
        <button
          class="c-modal__close"
          data-closemodal
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
  modalElement.id = id;
  modalElement.classList.add("c-modal");
  modalElement.setAttribute("aria-modal", "true");

  render(modalHtml, modalElement);

  return modalElement;
};

export const modal = ({
  slot,
  id,
}: {
  slot: TemplateResult;
  id: string;
}): Promise<void> => {
  const closeButtonRef: Ref<HTMLButtonElement> = createRef();
  const submitButtonRef: Ref<HTMLButtonElement> = createRef();

  const modalElement = createModal({
    slot,
    id,
    closeButtonRef,
    submitButtonRef,
  });
  document.body.appendChild(modalElement);

  return new Promise((resolve) => {
    // open modal
    modalElement.showModal();

    const closeButton = closeButtonRef.value;
    const submitButton = submitButtonRef.value;

    const close = () => {
      resolve();
      modalElement.close();
      modalElement.remove();
    };

    if (closeButton) {
      closeButton.onclick = close;
    }
    if (submitButton) {
      submitButton.onclick = close;
    }
  });
};
