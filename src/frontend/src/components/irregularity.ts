import { html, TemplateResult } from "lit-html";
import { warningIcon, closeIcon } from "./icons";

interface irregularityProps {
  message: string | TemplateResult;
  additionalClasses?: string[];
  errorType?: "warning" | "info" | "error";
  closeFn?: () => void;
}

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
