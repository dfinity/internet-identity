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
    <p>
      Internet Identity is the identity provider for the Internet Computer: A single service to log into many apps on the Internet Computer.
    </p>
    <p>
      Internet Identity protects your privacy. It will create a different identity for each application that you log into.
    </p>
    <p>
      Internet Identity works without passwords! You log in with your fingerprint, your face, or with dedicated security tokens. You can add multiple devices and security tokens and use your identities with any of them.
    </p>
    <p>
      Learn more with the <a href="todo">Internet Identity introduction</a>, and if you want to use the Internet Identity in your application, head for the <a href="todo">Developer documentation</a>.
    </p>
  </div>
  `;

export const aboutView = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
}
