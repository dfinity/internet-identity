/** A showcase for static pages. II pages are given a fake connection and loaded from here
 * just to give an idea of what they look like, and to speed up the development cycle when
 * working on HTML and CSS. */
import "./styles/main.css";
import { html, render } from "lit-html";
import { Connection } from "./utils/iiConnection";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { faqView } from "./flows/faq";
import { displayUserNumber } from "./flows/displayUserNumber";
import { loginKnownAnchor } from "./flows/login/knownAnchor";
import { loginUnknownAnchor } from "./flows/login/unknownAnchor";
import { pickRecoveryDevice } from "./flows/recovery/pickRecoveryDevice";
import { phraseRecoveryPage } from "./flows/recovery/recoverWith/phrase";
import { displayPage } from "./flows/authenticate";
import { DeviceData } from "../generated/internet_identity_types";
import { register, renderConstructing } from "./flows/register";

// A "dummy" connection which actually is just undefined, hoping pages won't call it
const dummyConnection = undefined as unknown as Connection;
const userNumber = BigInt(10000);

const recoveryPhrase: DeviceData = {
  alias: "Recovery Phrase",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { seed_phrase: null },
  purpose: { recovery: null },
  credential_id: [],
};

const recoveryDevice: DeviceData = {
  alias: "Recovery Device",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { unknown: null },
  purpose: { recovery: null },
  credential_id: [],
};

const defaultPage = () => {
  document.title = "Showcase";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};

const iiPages: Record<string, () => void> = {
  displayUserNumber: () => displayUserNumber(userNumber),
  faq: () => faqView(),
  about: () => aboutView(),
  compatibilityNotice: () => compatibilityNotice("This is the reason."),
  loginKnownAnchor: () => loginKnownAnchor(userNumber, dummyConnection),
  loginUnknownAnchor: () => loginUnknownAnchor(dummyConnection),
  pickRecoveryDevice: () =>
    pickRecoveryDevice([recoveryPhrase, recoveryDevice]),
  register: () => register(dummyConnection),
  authenticate: () => displayPage("https://nowhere.com", BigInt(10000)),
  recoverWithPhrase: () =>
    phraseRecoveryPage(userNumber, dummyConnection, recoveryPhrase),
  constructing: () => renderConstructing(),
};

// The showcase
const pageContent = html`
  <style>
    .showcase-grid {
      display: grid;
      list-style-type: none;
      width: 100vw;
      grid-template-columns: repeat(auto-fill, minmax(25rem, 1fr));
    }
    .showcase-grid > aside {
      position: relative;
      aspect-ratio: 0.75;
    }
    .showcase-grid a {
      position: absolute;
      inset: 0;
      overflow: hidden;
      outline: 1px solid #ccc;
    }

    .showcase-grid iframe {
      position: absolute;
      top: 0;
      left: 0;
      width: 200%;
      height: 200%;
      transform-origin: 0 0;
      transform: scale(0.5);
      border: none;
      pointer-events: none;
      overflow: hidden;
    }
    .showcase-grid h2 {
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      padding: 1rem;
      background: rgba(255, 255, 255, 0.25);
      margin: 0;
      color: #202124;
      backdrop-filter: blur(10px);
      box-shadow: 0 0 10px rgba(0, 0, 0, 0.25);
      font-size: 1.25rem;
    }
  </style>
  <h1>showcase</h1>
  <div class="showcase-grid">
    ${Object.entries(iiPages).map(([key, _]) => {
      return html`<aside>
        <a href="/${key}">
          <iframe src="/${key}" title="${key}"></iframe>
          <h2>${key}</h2>
        </a>
      </aside>`;
    })}
  </div>
`;

const init = async () => {
  // Some very simple content, just enough for the II pages to load correctly. The rest of the index.html
  // is provided in the webpack config by HtmlWebpackPlugin.
  const pageContent = html`
    <main id="pageContent" aria-live="polite"></main>
    <div id="loaderContainer"></div>
  `;

  render(pageContent, document.body);

  // We use the URL's path to route to the correct page.
  // If we can't find a page to route to, we just show the default page.
  // This is not very user friendly (in particular we don't show anything like a
  // 404) but this is an dev page anyway.
  const route = window.location.pathname.substring(1);
  const page = iiPages[route] ?? defaultPage;

  page();
};

init();
