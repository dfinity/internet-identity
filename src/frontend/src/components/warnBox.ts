import { html } from "lit-html";
import { warningIcon } from "./icons";

html`<svg slot="warnIcon">${warningIcon}</svg>`;

export const warnBox = (
  props = {
    title: "Warning",
    message: html`Somewhere unsafe string that can also contain html
      <b>Evil HTML</b>`,
    componentType: "div",
  }
) => {
  return html`${props.componentType === "div"
    ? html` <div class="warnBox">
        <div class="warnIcon">${warningIcon}</div>
        <div class="warnContent">
          <div class="warnTitle">${props.title}</div>
          <div class="warnMessage">${props.message}</div>
        </div>
      </div>`
    : html` <aside class="warnBox">
        <div class="warnIcon">${warningIcon}</div>
        <div class="warnContent">
          <div class="warnTitle">${props.title}</div>
          <div class="warnMessage">${props.message}</div>
        </div>
      </aside>`}`;
};
