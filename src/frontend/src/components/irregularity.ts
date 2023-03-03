import { html, TemplateResult } from "lit-html";
import { warningIcon, closeIcon } from "./icons";

interface irregularityProps {
  message: string | TemplateResult;
  additionalClasses?: string[];
  type?: "warning" | "info" | "error";
  closeFn?: () => void;
}

export const irregularity = ({
  message,
  additionalClasses = [],
  type = "error",
  closeFn,
}: irregularityProps): TemplateResult => {
  const cssClasses = ["c-irregularity"];

  if (additionalClasses.length > 0) {
    cssClasses.push(...additionalClasses);
  }

  if (typeof type === "string") {
    cssClasses.push(`c-irregularity--${type}`);
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
