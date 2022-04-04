import { html, render } from "lit-html";
import { compatibilityChart } from "../components/compatibilityChart";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = (reason: string) => html`
  <style>
    ul {
      list-style-type: none;
    }
  </style>
  <div class="container">
    <h1 id="compatibilityNotice">
      Your browser isn't supported for<br />Internet Identity
    </h1>
    <p>
      Unfortunately your browser doesn't support the necessary features that
      power your Internet Identity.<br />
    </p>
    <p>${reason}</p>
    ${compatibilityChart}
  </div>
`;

export const compatibilityNotice = (reason: string): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(reason), container);
};
