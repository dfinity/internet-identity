import { html, render, TemplateResult } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { withRef } from "../utils/lit-html";

/**
 * creates a modal that contains arbitrary content
 **/
export const createModal = ({ slot }: { slot: TemplateResult }) => {
  const modalElement: Ref<HTMLDialogElement> = createRef();

  // Close modal using the browser API
  const closeModal = () =>
    withRef(modalElement, (modalElement) => modalElement.close());

  // A container into which lit renders, which is removed from the DOM as soon as the dialog closes
  const container = document.createElement("div");
  const removeContainer = () => container.remove();

  // dialog role="dialog": closes modal on Esc
  // form method="dialog": closes modal on submit
  // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog
  const modalHtml = html`
    <dialog
      ${ref(modalElement)}
      @close=${() => removeContainer()}
      role="dialog"
      class="c-modal"
      aria-modal
    >
      <div class="c-modal__content c-card c-card--modal">
        <div class="c-modal__inner">
          <button
            @click=${() => closeModal()}
            class="c-modal__close"
            aria-label="Close Modal"
          >
            &times;
          </button>
          ${slot}
        </div>
      </div>
    </dialog>
  `;

  render(modalHtml, container);
  document.body.appendChild(container);
  withRef(modalElement, (modalElement) => modalElement.showModal());
};

// Create a modal with a form "OK" button, which resolves when "OK" is clicked
// (but _not_ when the form is closed via the close button)
export const formModal = ({ slot }: { slot: TemplateResult }): Promise<void> =>
  new Promise((resolve) =>
    createModal({
      slot: html`
        <form @submit=${() => resolve()} method="dialog">
          ${slot}
          <div class="c-modal__footer">
            <button type="submit" class="c-button c-button--primary">OK</button>
          </div>
        </form>
      `,
    })
  );
