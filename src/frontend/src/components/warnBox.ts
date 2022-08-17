import { html, TemplateResult } from "lit-html";
import { warningIcon } from "./icons";

// The Warning Component can be reused with following custom properties: title, message, and slot.
// Slot is for a custom element if needed, such as a button, that gets appended at the end.

interface warnBoxProps {
  title: string;
  message: string | TemplateResult;
  slot?: TemplateResult;
}

export const warnBox = ({
  title,
  message,
  slot,
}: warnBoxProps): TemplateResult =>
  html`<aside class="warnBox">
    <div class="warnIcon">${warningIcon}</div>
    <div class="warnContent">
      <div class="warnTitle">${title}</div>
      <div class="warnMessage">${message}</div>
      ${slot}
    </aside>
  </div>`;
