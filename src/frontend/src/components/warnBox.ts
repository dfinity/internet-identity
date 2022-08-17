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
}

export const warnBox = ({
  title,
  message,
  slot,
  htmlElement = "aside",
}: warnBoxProps): TemplateResult => {
  const contents: TemplateResult = html`
    <div class="warnIcon">${warningIcon}</div>
    <div class="warnContent">
      <div class="warnTitle">${title}</div>
      <div class="warnMessage">${message}</div>
      ${slot}
    </div>
  `;

  return html`${htmlElement === "aside"
    ? html` <aside class="warnBox">${contents}</aside>`
    : html` <div class="warnBox">${contents}</div> `}`;
};
