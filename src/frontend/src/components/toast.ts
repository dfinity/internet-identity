import { html, render } from "lit-html";
import { irregularity } from "./irregularity";

export const removeToast = (toast: HTMLElement): void => {
  toast.addEventListener("animationend", () => {
    toast.remove();
  });
  toast.classList.add("c-toast--closing");
};

const createToast = (messageStr: string): HTMLDivElement => {
  const message = html`${messageStr}`;
  const toastEl = document.createElement("div");
  toastEl.classList.add("c-toast");
  render(
    irregularity({
      message,
      closeFn: () => removeToast(toastEl),
    }),
    toastEl
  );
  return toastEl;
};

/**
 * Get the container for the toasts, or create it if it doesn't exist.
 * @returns the container reference for the toasts
 */
const getOrCreateToaster = (): Element | null => {
  const toasterEl = document.querySelector("[data-toaster]");
  if (!toasterEl) {
    // generic toaster that will be appended to the body
    const toasterEl = document.createElement("div");
    toasterEl.classList.add("c-toasts");
    // container for the toasts. Two reasons for this:
    // 1. we can reuse the l-container class for styling
    // 2. the toasts will only be as wide as the container (user can still interact with the rest of the page)
    const containerEl = document.createElement("div");
    containerEl.classList.add("l-container");
    containerEl.setAttribute("data-toasts", "");
    toasterEl.setAttribute("data-toaster", "");
    toasterEl.appendChild(containerEl);
    document.body.appendChild(toasterEl);
    return containerEl;
  }
  return toasterEl.querySelector("[data-toasts]");
};

/**
 * A lot like console.error, but with a toast.
 * use like: toast.error("Something went wrong");
 */
export const toast = {
  error: (message: string): void => {
    const toasterEl = getOrCreateToaster();
    const toastEl = createToast(message);
    toasterEl?.appendChild(toastEl);
  },
};
