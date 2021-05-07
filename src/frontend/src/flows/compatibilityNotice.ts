import { html, render } from "lit-html";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = html`
  <style>
    ul {
        list-style-type: none;
    }
  </style>
  <div class="container">
    <h1 id="compatibilityNotice">Your browser isn't supported for<br/>Internet Identity</h1>
    <p>Unfortunately your browser doesn't support the necessary features that power your Internet Identity.</p>
    <p>At this moment the following browsers are known to work:</p>
    <h3>For Desktop</h3>
    <ul>
    <li>Chrome version 67 or newer</li>
    <li>Firefox version 60 or newer</li>
    <li>Safari version 13 or newer</li>
    <li>Edge version 18 or newer</li>
    </ul>
    <h3>For Mobile</h3>
    <ul>
    <li>Chrome (Android)</li>
    <li>Safari 14.4 or newer (iOS)</li>
    <li>Firefox latest (iOS)</li>
    <li>Chrome latest (iOS)</li>
    </ul>
  </div>
  `;

export const compatibilityNotice = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
}