import { html, render } from "lit-html";
import { compatibilityChart } from "../components/compatibilityChart";
import { warnBox } from "../components/warnBox";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = (reason: string) => html`
  <div class="l-container c-card c-card--highlight">
    <h1 class="t-title t-title--main" id="compatibilityNotice">
      Internet Identity does not support your browser
    </h1>
    ${warnBox({
      title: "Reason",
      message: reason,
      additionalClasses: ["l-stack"],
    })}
    <div class="l-stack">${compatibilityChart}</div>
  </div>
`;

export const compatibilityNotice = (reason: string): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(reason), container);
};
