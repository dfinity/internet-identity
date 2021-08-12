import { html, render } from "lit-html";
import { compatibilityChart } from "../components/compatibilityChart";

// TODO: test for unique anchors
const faqs = [
  { question: "can I?", anchor: "can-i", answer: "oh yes you can" },
];

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = html`
  <style>
    ul {
      list-style-type: none;
    }
    :target {
      background-color: #000;
      transition: background-color 1s linear;
    }
  </style>
  <div class="container" id="faq">
    <h1>FAQ</h1>
    <ul>
      ${faqs.map(
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
