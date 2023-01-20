import { html, render } from "lit-html";
import { hydrate } from "lit-html/experimental-hydrate.js";
import { compatibilityData } from "../../components/compatibilityChart";
import { mainWindow } from "../../components/mainWindow";

const aboutContentSlot = html`
  <h1 class="t-title t-title--main">About</h1>
  <div class="l-stack">
    <h2 class="t-title">
      Anonymously connect to dapps on the Internet Computer
    </h2>
    <p class="t-lead">
      Use Internet Identity to manage your accounts on the IC without being
      tracked.
    </p>
  </div>
  <div class="l-stack">
    <h2 class="t-title"><a href="/">Create an Identity Anchor</a></h2>
    <p class="t-paragraph">
      Your Identity Anchor is a number that connects you to your accounts on
      applications across the Internet Computer. Store your Anchor somewhere
      safe. If you lose it, you lose access to all of the accounts that you
      created with it. Internet Identity keeps your anchor private and does not
      share it with any applications.
    </p>
  </div>
  <div class="l-stack">
    <h2 class="t-title">Easy and Secure Login</h2>
    <p class="t-paragraph">
      Get the privacy of many separate accounts, with the convenience of a
      single sign-on. Internet Identity generates a new private key for every
      dapp that you use, and stores the keys on your device. With just your
      Identity Anchor and device, you can manage hundreds of private keys and
      accounts.
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
          A decentralized, professional social media network that empowers users
          to own and control their identity and data.
        </p>
      </li>
      <li class="c-list__item">
        <a
          href="https://dscvr.ic0.app/"
          target="_blank"
          rel="noopener noreferrer"
          >Discovr</a
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
    <p class="t-paragraph">${compatibilityData.note}</p>

    <div class="l-horizontal l-stack">
      <div class="about__compatibility-flexchild">
        <h3 class="t-title">For Desktop</h3>
        <ul class="c-list c-list--bulleted l-stack">
          ${compatibilityData.desktop.map(
            (browser) => html`<li>${browser}</li>`
          )}
        </ul>
      </div>
      <div class="about__compatibility-flexchild">
        <h3 class="t-title">For Mobile</h3>
        <ul class="c-list c-list--bulleted l-stack">
          ${compatibilityData.mobile.map(
            (browser) => html`<li>${browser}</li>`
          )}
        </ul>
      </div>
    </div>
  </div>
`;

// The About page
export const pageContent = mainWindow({
  isWideContainer: true,
  slot: aboutContentSlot,
});

export const aboutView = (): void => {
  document.title = "About | Internet Identity";
  const container = document.getElementById("pageContent") as HTMLElement;
  if (process.env.HYDRATE_STATIC_PAGES !== "0") {
    hydrate(pageContent, container);
  }
  render(pageContent, container);
};
