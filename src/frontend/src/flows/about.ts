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
      The Internet Identity is the identity provider for the Internet Computer: One single service to log into many applications on the Internet Computer.
    </p>
    <p>
      The Internet Identity works without passwords! Instead, you can log in using your finger print, your face or using dedicated security tokens. You can configure multiple of these and use the Internet Identity with any of them, from all all your device.
    </p>
    <p>
      The Internet Identity protects your privacy. When you use the same Internet Identity account to log into different applications, you will be someone else to each of them.
    </p>
    <p>
      Learn more about this on the <a href="todo">the Internet Identity introduction</a>, and if you want to use the Internet Identity in your application, have a look at the <a href="todo">Developer documentation</a>.
    </p>
  </div>
  `;

export const aboutView = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
}
