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
      Internet Identity is the identity provider for the Internet Computer: A
      single service to log into apps on the Internet Computer.
    </p>
    <p>
      The Internet Identity service protects your privacy. When you register, you will not be asked to provide any personal information to the service or to the applications you access.
The service will create a different, random identity for each application you use.
    </p>
    <p>
      The Internet Identity service works without passwords! You log in with your own registered devices using their associated authentication methods.
    </p>
    <p>
      If you have questions about how to get started with Internet Identity, you
      can consult the
      <a href="http://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html"
        >Internet Identity guide</a
      >
      for step-by-step directions.
    </p>
  </div>
`;

export const aboutView = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};
