import { html, TemplateResult } from "lit-html";
import { TemplateElement } from "../utils/lit-html";
import { closeIcon, warningIcon } from "./icons";

interface irregularityProps {
  message: TemplateElement;
  closeFn: () => void;
}

/**
 * Irregularity component
 *
 * To use when something is out of the ordinary. For example, when a user has
 * not yet completed a task, or when a user has not yet verified their email
 * address. Or if there is an error with a form. It can also be used as a toast.
 *
 * It is a generic to use for all irregularities.
 *
 * @param {string} message - The message to display
 * @param {function} closeFn - When provided, the irregularity will be closable.
 *
 * For now we assume that all irregularities are errors. In the future, we may
 * want to add a type parameter to this component.
 **/
export const irregularity = ({
  message,
  closeFn,
}: irregularityProps): TemplateResult => {
  const cssClasses = [
    "c-irregularity",
    "c-irregularity--error",
    "c-irregularity--closable",
  ];

  const contents: TemplateResult = html`
    <div class="${cssClasses.join(" ")}">
      <div class="c-irregularity__icon">${warningIcon}</div>
      <p class="c-irregularity__message">${message}</p>
      <button
        class="c-irregularity__close"
        aria-label="Close"
        @click="${() => closeFn()}"
      >
        ${closeIcon}
      </button>
    </div>
  `;

  return contents;
};
