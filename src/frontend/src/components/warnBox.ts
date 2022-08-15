import { html, TemplateResult } from "lit-html";
import { warningIcon } from "./icons";

interface warnBoxProps {
  title: string;
  message: string | TemplateResult;
  componentType: string;
  slot?: TemplateResult;
}

export const warnBox = (
  props: warnBoxProps = {
    title: "Warning",
    message: html`Somewhere unsafe string that can also contain html
      <b>Evil HTML</b>`,
    componentType: "div",
    slot: html`<button></button>`,
  }
): TemplateResult =>
  html`${props.componentType === "div"
    ? html` <div class="warnBox">
        <div class="warnIcon">${warningIcon}</div>
        <div class="warnContent">
          <div class="warnTitle">${props.title}</div>
          <div class="warnMessage">${props.message}</div>
          ${props.slot}
        </div>
      </div>`
    : html` <aside class="warnBox">
        <div class="warnIcon">${warningIcon}</div>
        <div class="warnContent">
          <div class="warnTitle">${props.title}</div>
          <div class="warnMessage">${props.message}</div>
          ${props.slot}
        </div>
      </aside>`}`;
