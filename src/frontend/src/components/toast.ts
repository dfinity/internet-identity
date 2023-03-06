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
  const $toast = document.createElement("div");
  $toast.classList.add("c-toast");
  render(
    irregularity({
      message,
      closeFn: () => removeToast($toast),
    }),
    $toast
  );
  return $toast;
};

/**
 * Get the container for the toasts, or create it if it doesn't exist.
 * @returns the container reference for the toasts
 */
const getOrCreateToaster = (): Element | null => {
  const $toaster = document.querySelector("[data-toaster]");
  if (!$toaster) {
    const $toaster = document.createElement("div");
    $toaster.classList.add("c-toasts");
    const $container = document.createElement("div");
    $container.classList.add("l-container");
    $container.setAttribute("data-toasts", "");
    $toaster.setAttribute("data-toaster", "");
    $toaster.appendChild($container);
    document.body.appendChild($toaster);
    return $container;
  }
  return $toaster.querySelector("[data-toasts]");
};

/**
 * A lot like console.error, but with a toast.
 * use like: toast.error("Something went wrong");
 */
export const toast = {
  error: (message: string): void => {
    const $toaster = getOrCreateToaster();
    const $toast = createToast(message);
    $toaster?.appendChild($toast);
  },
};
