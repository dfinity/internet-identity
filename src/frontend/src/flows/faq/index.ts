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
    <summary class="font-bold font-sans">
      ${faq.question}
    <div class="gradient-decoration-small"></div>
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

    .gradient-decoration {
        background: -webkit-gradient(linear,left top,right top,from(#29abe2),color-stop(33%,#fbb03b),color-stop(66%,#f15a24),to(#ed1e79));
        background: linear-gradient(90deg,#29abe2,#fbb03b 33%,#f15a24 66%,#ed1e79);
        height: 2px;
        /*position: absolute; */
        bottom: 0;
        left: 0;
        right: 0;
        margin: 2em 5em;
    }

    .gradient-decoration-small {
        background: -webkit-gradient(linear,left top,right top,from(#29abe2),color-stop(33%,#fbb03b),color-stop(66%,#f15a24),to(#ed1e79));
        background: linear-gradient(90deg,#29abe2,#fbb03b 33%,#f15a24 66%,#ed1e79);
        height: 1px;
        /*position: absolute; */
        bottom: 0;
        left: 0;
        right: 0;
    }

    :target {
      animation-name: flash-question;
      animation-duration: 600ms;
    }
  </style>
  <div class="container p-6 mx-auto h-full bg-gray-100 text-gray-500 max-w-2xl">
  <div>
    <h1 class="text-7xl baba text-center font-bold tracking-wide p-8 text-gray-700">
      FAQ
    </h1>
    <div class="gradient-decoration"></div>
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
