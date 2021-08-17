import { html, render } from "lit-html";
import { compatibilityChart } from "../../components/compatibilityChart";

import { questions } from "./questions";

// re-export for ease of use
export { questions } from "./questions";

// The FAQ page
const pageContent = html`
  <style>
    /* briefly flash the question when redirected to a particular question */
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

    a {
      color: var(--grey-800);
    }

    ul {
      list-style-type: none;
    }

    li {
      border-radius: 0.5rem;
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
            <ul class="links-list">
              ${Object.values(faq.links).map(
                (link) =>
                  html`<li>
                    &middot;
                    <a class="textLink" rel="noopener noreferrer" href="${link.link}" target="_blank"
                      >${link.name}</a
                    >
                  </li>`
              )}
            </ul>
          </li>`
      )}
    </ul>
  </div>
`;

export const faqView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};
