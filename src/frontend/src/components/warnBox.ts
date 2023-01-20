import { html, TemplateResult } from "lit-html";
import { warningIcon } from "./icons";

// The Warning Component can be reused with following custom properties: title, message, slot and htmlElement.
// "slot" is for a custom element, such as a button, that gets appended at the end.
// "htmlElement" is the type of wrapper element the component will be.
// By default the component will be an <aside>. The other option is a <div> wrapper.

interface warnBoxProps {
  title: string;
  message: string | TemplateResult;
  slot?: TemplateResult;
  htmlElement?: "div" | "aside";
  additionalClasses?: string[];
}

export const warnBox = ({
  title,
  message,
  slot,
  htmlElement = "aside",
  additionalClasses = [],
}: warnBoxProps): TemplateResult => {
  const cssClasses = ["c-card", "c-card--narrow", "c-card--warning"];
  if (additionalClasses.length > 0) {
    cssClasses.push(...additionalClasses);
  }
  const contents: TemplateResult = html`
    <span class="c-card__icon" aria-hidden="true">${warningIcon}</span>
    <div class="c-card__content">
      <h3 class="t-title c-card__title">${title}</h3>
      <div data-role="warning-message" class="t-paragraph c-card__paragraph">
        ${message}
      </div>
      ${slot}
    </div>
  `;

  return html`${htmlElement === "aside"
    ? html` <aside class="${cssClasses.join(" ")}">${contents}</aside>`
    : html` <div class="${cssClasses.join(" ")}">${contents}</div> `}`;
};
