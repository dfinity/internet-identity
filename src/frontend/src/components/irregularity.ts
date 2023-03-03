import { html, TemplateResult } from "lit-html";
import { warningIcon } from "./icons";

interface warnBoxProps {
  message: string | TemplateResult;
  additionalClasses?: string[];
  type: "warning" | "info" | "error";
  isClosable?: boolean;
}

export const irregularity = ({
  message,
  additionalClasses = [],
  type = "error",
  isClosable = false,
}: warnBoxProps): TemplateResult => {
  const cssClasses = ["c-irregularity"];

  if (additionalClasses.length > 0) {
    cssClasses.push(...additionalClasses);
  }

  if (type) {
    cssClasses.push(`c-irregularity--${type}`);
  }

  if (isClosable) {
    cssClasses.push(`c-irregularity--closable`);
  }

  const contents: TemplateResult = html`
    <div class="${cssClasses.join(" ")}">
      <div class="c-irregularity__icon">${warningIcon}</div>
        <p class="c-irregularity__message">
          ${message}
        </p>
      </div>
    </div>
  `;

  return contents;
};
