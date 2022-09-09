import { html, render } from "lit-html";

// The About page
export const footerLinksContent = html`
  <a href="/about" class="c-button info-button">ℹ</a>

  <aside aria-label="Other actions">
    <ul class="t-discreet c-list c-list--inline">
      <li class="textLink" id="registerSection">
        <a id="registerButton" class="t-link">Create New Anchor</a>
      </li>
      <li class="textLink">
        <a id="addNewDeviceButton" class="t-link">Associate Device</a>
      </li>
      <li class="textLink">
        <a id="browserCompatibilityButton" class="t-link"
          >Browser Compatibility</a
        >
      </li>
      <li class="textLink">
        <a id="faqLink" class="t-link" href="/faq">FAQ</a>
      </li>
      <li class="textLink">
        <a id="recoverButton" class="t-link">Lost Access?</a>
      </li>
    </ul>
  </aside>
`;

export const footerLinksView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(footerLinksContent, container);
};
