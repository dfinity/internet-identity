import { html, TemplateResult, render } from "lit-html";

/**
 * creates a modal that contains arbitrary content
 * and ID is obligatory for accessibility reasons
 * use crypto.randomUUID() if you feel uninspired
 **/
export const createModal = ({
  slot,
  id,
}: {
  slot: TemplateResult;
  id: string;
}): HTMLDialogElement => {
  const modalHtml = html`
    <form method="dialog" class="c-modal__content c-card c-card--modal">
      <div class="c-modal__inner">
        <span class="c-modal__close" data-closemodal>&times;</span>
        ${slot}
        <div class="c-modal__footer">
          <button type="submit" class="c-button c-button--primary">OK</button>
        </div>
      </div>
    </form>
  `;

  const $modal = document.createElement("dialog");
  $modal.id = id;
  $modal.classList.add("c-modal");
  $modal.setAttribute("aria-modal", "true");

  render(modalHtml, $modal);
  return $modal;
};

export const modal = ({
  slot,
  id,
}: {
  slot: TemplateResult;
  id: string;
}): Promise<void> => {
  const $modal = createModal({ slot, id });
  document.body.appendChild($modal);

  return new Promise((resolve) => {
    // open modal
    $modal.showModal();
    const $modalClose = $modal.querySelector(
      "[data-closemodal]"
    ) as HTMLElement;
    const $modalSubmit = $modal.querySelector(
      "button[type=submit]"
    ) as HTMLButtonElement;

    const close = () => {
      resolve();
      $modal.close();
      $modal.remove();
    };

    $modalClose.onclick = close;
    $modalSubmit.onclick = close;
  });
};
