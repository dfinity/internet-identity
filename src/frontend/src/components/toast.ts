import { html, render, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { repeat } from "lit/directives/repeat.js";
import { TemplateElement } from "../utils/lit-html";
import { Chan } from "../utils/utils";
import { closeIcon, warningIcon } from "./icons";

// A toast element containing a message, and the (static) list of all toasts. The `Toast` element
// must be an object because we (lit) use object-references in `repeat` to figure out which DOM elements
// to replace.
type Toast = { message: TemplateElement };
const toasts: Toast[] = []; // the "toasts" singleton

/**
 * A lot like console.error, but with a toast.
 * use like: toast.error("Something went wrong");
 */
export const toast = {
  error: (message: Toast["message"]): void => {
    toasts.push({ message });
    renderToasts();
  },
};

// Render or rerender toasts.
const renderToasts = () => {
  render(
    html` <div class="c-toasts l-container">
      ${repeat(toasts, (x) => x, toastTemplate)}
    </div>`,
    document.body
  );
};

const toastTemplate = (toast: Toast): TemplateResult => {
  const closing = new Chan<boolean>(false);
  const closeToast = () => closing.send(true);

  const removeToast = () => {
    const toastIndex = toasts.indexOf(toast);
    if (toastIndex > -1) {
      toasts.splice(toastIndex, 1);
    }
    renderToasts();
  };

  return html` <div
    class="c-toast ${asyncReplace(
      closing.map((closing) => (closing ? "c-toast--closing" : undefined))
    )}"
    @animationend=${asyncReplace(
      closing.map((closing) => (closing ? () => removeToast() : undefined))
    )}
  >
    <div class="c-irregularity c-irregularity--error c-irregularity--closable">
      <div class="c-irregularity__icon">${warningIcon}</div>
      <p class="c-irregularity__message">${toast.message}</p>
      <button
        class="c-irregularity__close"
        aria-label="Close"
        @click="${() => closeToast()}"
      >
        ${closeIcon}
      </button>
    </div>
  </div>`;
};
