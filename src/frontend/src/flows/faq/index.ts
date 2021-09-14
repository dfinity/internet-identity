import { html, render } from "lit-html";
import "../../styles/main.css";

import { questions } from "./questions";
import type { Question } from "./questions";

// re-export for ease of use
export { questions } from "./questions";

// The rendered (list item) question
function renderQuestion(faq: Question) {
  return html`<li
    class="faq__question"
  >
    <details
    id=${faq.anchor} >
    <summary class="faq__question-summary">
      ${faq.question}
    <div class="faq__question-underline"></div>
    </summary>
    <div>
      <p class="faq__answer">${faq.answer}</p>
      <ul class="faq__answer-links">
        ${Object.values(faq.links).map(
          (link) =>
            html`<li>
              &middot;
              <a
                class="faq__answer-link"
                rel="noopener noreferrer"
                href="${link.link}"
                target="_blank"
                >${link.name} &#8599;</a
              >
            </li>`
        )}
      </ul>
    </div>
  </li>`;
}

// The FAQ page
const pageContent = html`
  <style>
    html,
    body,
    main {
      height: max-content;
      overflow-x: hidden;
    }

    body {
      position: relative;
    }
    /* briefly flash the question when redirected to a particular question */
    @keyframes flash-question {
      0% {
        background-color: transparent;
      }
      50% {
        background-color: var(--rainbow-orange);
        border-radius: 0.3em;
      }
      100% {
        background-color: transparent;
      }
    }
    :target {
      animation-name: flash-question;
      animation-duration: 600ms;
    }
  </style>
  <div class="faq__container">
    <div>
      <h1 class="faq__title">FAQ</h1>
      <div class="faq__title-underline"></div>
      <ul class="faq__questions">
        ${Object.values(questions).map((faq) => renderQuestion(faq))}
      </ul>
    </div>
  </div>
`;

const openAnchor = (): void => {
  const hash = location.hash.substring(1);
  if (hash) {
    const details = document.getElementById(hash) as HTMLDetailsElement;
    details.open = true;
  }
};

export const faqView = (): void => {
  document.title = "FAQ | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  openAnchor(); // needs to happen after DOM was rendered
};

faqView();
