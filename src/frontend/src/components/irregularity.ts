import { html, TemplateResult } from "lit-html";
import { warningIcon, closeIcon } from "./icons";

interface irregularityProps {
  message: string | TemplateResult;
  additionalClasses?: string[];
  errorType?: "warning" | "info" | "error";
  closeFn?: () => void;
}

/**
 * Irregularity component
 *
 * To use when something is out of the ordinary. For example, when a user has
 * not yet completed a task, or when a user has not yet verified their email
 * address. Or if there is an error with a form. It can also be used as a tost.
 *
 * It is a generic to use for all irregularities.
 *
 * @param {string} message - The message to display
 * @param {string[]} additionalClasses - Additional classes to add to the component
 * @param {string} errorType - The type of irregularity. Can be "warning", "info" or "error"
 * @param {function} closeFn - When provided, the irregularity will be closable.
 **/
export const irregularity = ({
  message,
  additionalClasses = [],
  errorType = "error",
  closeFn,
}: irregularityProps): TemplateResult => {
  const cssClasses = ["c-irregularity"];

  if (additionalClasses.length > 0) {
    cssClasses.push(...additionalClasses);
  }

  if (typeof errorType === "string") {
    cssClasses.push(`c-irregularity--${errorType}`);
  }

  if (typeof closeFn === "function") {
    cssClasses.push(`c-irregularity--closable`);
  }

  const contents: TemplateResult = html`
    <div class="${cssClasses.join(" ")}">
      <div class="c-irregularity__icon">${warningIcon}</div>
      <p class="c-irregularity__message">${message}</p>
      ${typeof closeFn === "function"
        ? html`<button
            class="c-irregularity__close"
            aria-label="Close"
            @click="${() => closeFn()}"
          >
            ${closeIcon}
          </button>`
        : null}
    </div>
  `;

  return contents;
};
