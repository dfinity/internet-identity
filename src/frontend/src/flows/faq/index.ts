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
        background-color: rgba(237,186,152,1);
        border-radius: 0.3em;;
      }
      100% {
        background-color: transparent;
      }
    }
    ul {
      list-style-type: none;
      width: auto;
    }

    @media only screen and (max-width: 600px) {
        .faq__container {
            padding: 0.6rem;
        }
    }
    @media only screen and (min-width: 600px) {
        .faq__container {
            padding: 4rem;
        }
    }

    .faq__questions {
        padding: 0;
    }

    .faq__question {
        padding: 1.5rem 0;
    }

    .faq__question-underline {
        background: -webkit-gradient(linear,left top,right top,from(#29abe2),color-stop(33%,#fbb03b),color-stop(66%,#f15a24),to(#ed1e79));
        background: linear-gradient(90deg,#29abe2,#fbb03b 33%,#f15a24 66%,#ed1e79);
        height: 1px;
        /*position: absolute; */
        bottom: 0;
        left: 0;
        right: 0;
        margin: auto;
    }
    .faq__question-summary {
        font-weight: 700;
        font-family: ui-sans-serif,system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";

    }

    .faq__title {
        color: rgba(55,65,81,1);
        font-weight: 700;
        font-size: 4.5rem;
        line-height: 1;
        text-align: center;
        letter-spacing: 0.025em;
        padding: 2rem;
    }

    .faq__title-underline {
        background: -webkit-gradient(linear,left top,right top,from(#29abe2),color-stop(33%,#fbb03b),color-stop(66%,#f15a24),to(#ed1e79));
        background: linear-gradient(90deg,#29abe2,#fbb03b 33%,#f15a24 66%,#ed1e79);
        height: 2px;
        /*position: absolute; */
        bottom: 0;
        left: 0;
        right: 0;
        margin: 2em 5em;
    }

    .faq__question-details-container {
        height: max-content;
    }

    .faq__answer {
        line-height: 2rem;
        font-weight: 200;
        max-width: 28rem;
        padding: 1.2em;
        margin: auto 0;
    }

    .faq__answer-links {
        padding: 1rem;
    }

    .faq__answer-link {
        color: rgba(31,41,55,1);
    }

    .faq__container {
        background-color: rgba(243,244,246,1);
        max-width: 42rem;
    }

    :target {
      animation-name: flash-question;
      animation-duration: 600ms;
    }
  </style>
  <div class="faq__container">
  <div>
    <h1 class="faq__title">
      FAQ
    </h1>
    <div class="faq__title-underline"></div>
    <ul class="faq__questions">
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
  document.title = "FAQ | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  openAnchor(); // needs to happen after DOM was rendered
};

faqView();
