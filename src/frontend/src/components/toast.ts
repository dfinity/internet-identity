import { TemplateElement } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { html, render, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { repeat } from "lit-html/directives/repeat.js";
import { checkmarkIcon, closeIcon, settingsIcon, warningIcon } from "./icons";

// A toast element containing a message, and the (static) list of all toasts. The `Toast` element
// must be an object because we (lit) use object-references in `repeat` to figure out which DOM elements
// to replace.
type Toast = { message: TemplateElement; level: "error" | "info" | "success" };
const toastStore: Toast[] = []; // the "toasts" singleton

/**
 * A lot like console.error, but with a toast.
 * use like: toast.error("Something went wrong");
 */
export const toast = {
  error: (message: Toast["message"]): void => {
    toastStore.push({ message, level: "error" });
    renderToasts();
  },
  info: (message: Toast["message"]): void => {
    toastStore.push({ message, level: "info" });
    renderToasts();
  },
  success: (message: Toast["message"]): void => {
    toastStore.push({ message, level: "success" });
    renderToasts();
  },
};

// Render or rerender toasts.
const renderToasts = () => {
  render(
    html` <div class="c-toasts l-container">
      ${repeat(toastStore, (x) => x, toastTemplate)}
    </div>`,
    document.body
  );
};

const toastTemplate = (toast: Toast): TemplateResult => {
  const closing = new Chan<boolean>(false);

  // Trigger the closing animation
  const closeToast = () => closing.send(true);

  // Remove the toast from the store
  const removeToast = () => {
    const toastIndex = toastStore.indexOf(toast);
    if (toastIndex > -1) {
      toastStore.splice(toastIndex, 1);
    }
    renderToasts();
  };

  const levelClass = {
    error: "c-toast-body--error",
    info: "c-toast-body--info",
    success: "c-toast-body--success",
  }[toast.level];
  const levelIcon = {
    error: warningIcon,
    info: settingsIcon,
    success: checkmarkIcon,
  }[toast.level];

  return html` <div
    role="alert"
    class="c-toast ${asyncReplace(
      closing.map((closing) => (closing ? "c-toast--closing" : undefined))
    )}"
    @animationend=${asyncReplace(
      closing.map((closing) => (closing ? () => removeToast() : undefined))
    )}
  >
    <div class="c-toast-body ${levelClass}">
      <div class="c-toast-body__icon">${levelIcon}</div>
      <p class="c-toast-body__message">${toast.message}</p>
      <button
        class="c-toast-body__close"
        aria-label="Close"
        @click="${() => closeToast()}"
      >
        ${closeIcon}
      </button>
    </div>
  </div>`;
};
