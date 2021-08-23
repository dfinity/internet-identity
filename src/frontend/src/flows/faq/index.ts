import { html, render } from "lit-html";

import { questions } from "./questions";
import type { Question } from "./questions";

// re-export for ease of use
export { questions } from "./questions";

// The rendered (list item) question
function renderQuestion(faq: Question) {
  return html`<li
    id=${faq.anchor}
    class="sm:mx-4 sm:mb-10 sm:border-b-0 border-b-2 border-blue-400 sm:ring-2 sm:rounded-md p-4"
  >
    <div class="font-bold font-sans border-b-2 w-full border-blue-400">
      ${faq.question}
    </div>
    <div class="pl-4 pt-4">
      <p>${faq.answer}</p>
      <ul class="p-2">
        ${Object.values(faq.links).map(
          (link) =>
            html`<li>
              &middot;
              <a
                class="text-blue-800"
                rel="noopener noreferrer"
                href="${link.link}"
                target="_blank"
                >${link.name}</a
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
    /* briefly flash the question when redirected to a particular question */
    @keyframes flash-question {
      0% {
        background-color: transparent;
      }
      50% {
        background-color: grey;
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

    :target {
      animation-name: flash-question;
      animation-duration: 3s;
    }
  </style>
  <div
    class="container rounded-md m-auto h-full sm:my-10 bg-blue-100 sm:shadow-md text-blue-500 max-w-2xl"
  >
    <h1 class="text-2xl text-center sm:text-left font-bold tracking-wide p-4">
      FAQ
    </h1>
    <ul>
      ${Object.values(questions).map((faq) => renderQuestion(faq))}
    </ul>
  </div>
`;

export const faqView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};

faqView();
