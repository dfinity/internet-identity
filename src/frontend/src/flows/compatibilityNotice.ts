import { html, render } from "lit-html";
import { compatibilityChart } from "../components/compatibilityChart";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = (reason: string) => html`
  <div class="l-container c-card c-card--highlight">
    <h1 class="t-title t-title--main" id="compatibilityNotice">
      Your browser isn't supported for Internet Identity
    </h1>
    <p class="t-lead">
      Unfortunately your browser doesn't support the necessary features that
      power your Internet Identity.<br />
    </p>
    <p class="t-paragraph">${reason}</p>
    ${compatibilityChart}
  </div>
`;

export const compatibilityNotice = (reason: string): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(reason), container);
};
