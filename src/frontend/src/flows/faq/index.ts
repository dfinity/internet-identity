import { html, render } from "lit-html";
import { compatibilityChart } from "../../components/compatibilityChart";

import { questions } from "./questions";

// re-export for ease of use
export { questions } from "./questions";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = html`
  <style>
    @keyframes example {
      0% {
        background-color: transparent;
      }
      50% {
        background-color: var(--grey-100);
      }
      100% {
        background-color: transparent;
      }
    }

    ul {
      list-style-type: none;
    }
    li {
      border-radius: 1rem;
      padding: 0.5rem;
    }
    :target {
      animation-name: example;
      animation-duration: 3s;
    }
  </style>
  <div class="container" id="faq">
    <h1>FAQ</h1>
    <ul>
      ${Object.values(questions).map(
        (faq) =>
          html`<li id=${faq.anchor}>
            <h3>${faq.question}</h3>
            <p>${faq.answer}</p>
          </li>`
      )}
    </ul>
    <h2>Compatibility</h2>
    ${compatibilityChart}
  </div>
`;

export const faqView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};
