import { html, render } from "lit-html";
import { hydrate } from "lit-html/experimental-hydrate.js";
import { compatibilityData } from "../../components/compatibilityChart";
import { mainWindow } from "../../components/mainWindow";

const aboutContentSlot = () => {
  return html`
    <h1 class="t-title t-title--main">About</h1>
    <div class="l-stack">
      <h2 class="t-title">
        Securely Connect to dapps on the Internet Computer
      </h2>
      <p class="t-lead">
        Use Internet Identity to manage your accounts on the IC without being
        tracked.
      </p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">Identity Anchors</h2>
      <p class="t-paragraph">
        Your Identity Anchor is a number that connects you to your accounts on
        applications across the Internet Computer. Store your Anchor somewhere
        safe. If you lose it, you lose access to all of the accounts that you
        created with it. Internet Identity keeps your anchor private and does
        not share it with any applications.<br /><br />
        <a href="/">Create an Identity Anchor now!</a>
      </p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">Easy and Secure Login</h2>
      <p class="t-paragraph">
        Unlock authentication with your device via FaceID, fingerprint sensor or
        use a YubiKey. This is as easy as unlocking your phone and does not
        require managing passwords. At the same time it provides strong
        security, as the cryptographic key never leaves your device. Remain
        pseudonymous using the Internet Identity authentication framework, which
        prevents user tracking across dapps and services.
      </p>
    </div>
    <div class="l-stack">
      <h2 class="t-title">Explore dapps on the Internet Computer</h2>
      <p class="t-paragraph">
        With an Identity Anchor, you can create accounts with dapps on the
        Internet Computer, like:
      </p>
      <ul class="c-list c-list--bulleted l-stack">
        <li class="c-list__item">
          <a
            href="https://distrikt.app/"
            target="_blank"
            rel="noopener noreferrer"
            >Distrikt</a
          >
          <p>
            A decentralized, professional social media network that empowers
            users to own and control their identity and data.
          </p>
        </li>
        <li class="c-list__item">
          <a href="https://dscvr.one/" target="_blank" rel="noopener noreferrer"
            >DSCVR</a
          >
          <p>
            A decentralized social media platform that rewards users for their
            contributions.
          </p>
        </li>
        <li class="c-list__item">
          <a href="https://oc.app/" target="_blank" rel="noopener noreferrer">
            OpenChat
          </a>
          <p>
            OpenChat is a fully featured chat application running end-to-end on
            the Internet Computer blockchain.
          </p>
        </li>
      </ul>
    </div>
    <div class="l-stack">
      <h2 class="t-title">Compatibility</h2>

      <div class="l-horizontal l-stack">
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">Recommended Desktop</h3>
          <ul class="c-list c-list--bulleted l-stack">
            ${compatibilityData.desktop.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
        <div class="about__compatibility-flexchild">
          <h3 class="t-title">Recommended Mobile</h3>
          <ul class="c-list c-list--bulleted l-stack">
            ${compatibilityData.mobile.map(
              (browser) => html`<li>${browser}</li>`
            )}
          </ul>
        </div>
      </div>
      <p class="t-paragraph">${compatibilityData.note}</p>
      <p class="t-paragraph">${compatibilityData.note2}</p>
    </div>
  `;
};

// The About page
export const pageContent = () =>
  mainWindow({
    isWideContainer: true,
    slot: aboutContentSlot,
  });

export const aboutView = (): void => {
  document.title = "About | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  if (process.env.HYDRATE_STATIC_PAGES !== "0") {
    hydrate(pageContent(), container);
  }
  render(pageContent(), container);
};
