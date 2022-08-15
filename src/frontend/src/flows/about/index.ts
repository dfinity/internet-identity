import { html, render } from "lit-html";
import { compatibilityData } from "../../components/compatibilityChart";

// The About page
const pageContent = html`
  <div class="l-container l-container--wide c-card c-card--highlight">
    <h1 class="t-title t-title--main">About</h1>
    <div class="l-section">
      <h2 class="t-title">Internet Identity</h2>
      <p class="t-paragraph">
        Internet Identity is the identity provider for the Internet Computer: A
        dapp facilitating authentication on the Internet Computer.
      </p>
      <p class="t-paragraph">
        For frequently asked questions, check the
        <a href="/faq" title="Go to the Internet Identity FAQ page">FAQ page</a
        >.
      </p>
    </div>
    <div class="l-section">
      <h2 class="t-title">Compatibility</h2>
      <p class="t-paragraph">${compatibilityData.note}</p>

      <div class="l-horizontal l-section">
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">For Desktop</h3>
          <ul class="c-list c-list--bulleted l-section">
            ${compatibilityData.desktop.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">For Mobile</h3>
          <ul class="c-list c-list--bulleted l-section">
            ${compatibilityData.mobile.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
      </div>
    </div>
  </div>
`;

export const aboutView = (): void => {
  document.title = "About | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};
