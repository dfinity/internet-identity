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
      Internet Identity protects your privacy. It will create a different
      identity for each application that you log into.
    </p>
    <p>
      Internet Identity works without passwords! You log in with your
      fingerprint, your face, or with dedicated security tokens. You can add
      multiple devices and security tokens and use your identities with any of
      them.
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
