import { mainWindow } from "$lib/templates/mainWindow";
import { warnBox } from "$lib/templates/warnBox";
import { html, render } from "lit-html";

// Taken from: https://caniuse.com/?search=PublicKeyCredential
const pageContent = () =>
  warnBox({
    title: "Browser Not Supported",
    message: html`
      <p class="t-paragraph">
        Internet Identity is not supported by your browser.
      </p>
      <p class="t-paragraph">
        This device does not offer
        <a
          href="https://fidoalliance.org/passkeys/"
          rel="noopener noreferrer"
          class="t-link"
          >Passkeys</a
        >
        authentication. Please make sure you have biometrics (fingerprint /
        Touch ID / Face ID) enabled and try again.
      </p>
    `,
    additionalClasses: ["l-stack"],
  });
export const compatibilityNotice = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(
    mainWindow({
      slot: pageContent(),
      showLogo: false,
    }),
    container,
  );
};
