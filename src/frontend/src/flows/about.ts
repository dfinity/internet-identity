import { html, render } from "lit-html";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = html`
  <style>
    ul {
        list-style-type: none;
    }
  </style>
  <div class="container" id="about">
    <h1>About the Internet Identity</h1>
    <p>Lorem</p>
    <p>Ipsum</p>
  </div>
  `;

export const aboutView = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
}
