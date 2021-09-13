import { html, render } from "lit-html";

import { questions } from "./questions";
import type { Question } from "./questions";

// re-export for ease of use
export { questions } from "./questions";

// The rendered (list item) question
function renderQuestion(faq: Question) {
  return html`<li
    class="py-8"
  >
    <details
    id=${faq.anchor} >
    <summary class="font-bold font-sans border-b-2 w-full border-green-200">
      ${faq.question}
    </summary>
    <div class="p-6">
      <p class="leading-8 font-extralight max-w-md">${faq.answer}</p>
      <ul class="p-4">
        ${Object.values(faq.links).map(
          (link) =>
            html`<li>
              &middot;
              <a
                class="text-gray-800"
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
    }
    /* briefly flash the question when redirected to a particular question */
    @keyframes flash-question {
      0% {
        background-color: transparent;
      }
      50% {
        background-color: #a7f3d0;
      }
      100% {
        background-color: transparent;
      }
    }
    ul {
      list-style-type: none;
    }

    :target {
      animation-name: flash-question;
      animation-duration: 600ms;
    }
  </style>
  <div class="container p-6 mx-auto h-full bg-gray-100 text-gray-500 max-w-2xl">
    <h1 class="text-7xl text-center font-bold tracking-wide p-8 text-green-300">
      FAQ
    </h1>
    <ul class="px-6">
      ${Object.values(questions).map((faq) => renderQuestion(faq))}
    </ul>
  </div>
`;

const openAnchor = (): void => {
  var hash = location.hash.substring(1);
  if (hash) {
    var details = document.getElementById(hash) as HTMLDetailsElement;
    if (details) {
      details.open = true;
    }
  }
};

export const faqView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  openAnchor(); // needs to happen after DOM was rendered
};

faqView();
