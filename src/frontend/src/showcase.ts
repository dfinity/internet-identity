/** A showcase for static pages. II pages are given a fake connection and loaded from here
 * just to give an idea of what they look like, and to speed up the development cycle when
 * working on HTML and CSS. */
import "./styles/main.css";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Chan, NonEmptyArray, asNonEmptyArray } from "./utils/utils";
import { I18n } from "./utils/i18n";
import { withRef, mount } from "./utils/lit-html";
import { TemplateResult, html, render } from "lit-html";
import {
  Challenge,
  DeviceData,
  Timestamp,
} from "../generated/internet_identity_types";
import { AuthenticatedConnection } from "./utils/iiConnection";
import { styleguide } from "./styleguide";
import { compatibilityNotice } from "./flows/compatibilityNotice";
import { aboutView } from "./flows/about";
import { showWarning } from "./banner";
import { pickRecoveryDevice } from "./flows/recovery/pickRecoveryDevice";
import { displaySeedPhrase } from "./flows/recovery/displaySeedPhrase";
import { phraseRecoveryPage } from "./flows/recovery/recoverWith/phrase";
import { deviceRecoveryPage } from "./flows/recovery/recoverWith/device";
import { authnPages } from "./components/authenticateBox";
import { authnTemplateAuthorize } from "./flows/authorize";
import { promptDeviceAliasPage } from "./flows/register/alias";
import { renderConstructing } from "./flows/register/construct";
import { promptCaptchaPage, badChallenge } from "./flows/register/captcha";
import { displayUserNumberPage } from "./flows/register/finish";
import { chooseRecoveryMechanismPage } from "./flows/recovery/chooseRecoveryMechanism";
import { displaySingleDeviceWarning } from "./flows/recovery/displaySingleDeviceWarning";
import { displayManagePage, authnTemplateManage } from "./flows/manage";
import { chooseDeviceAddFlow } from "./flows/addDevice/manage";
import { pickDeviceAliasPage } from "./flows/addDevice/manage/addDevicePickAlias";
import { deviceSettingsPage } from "./flows/manage/deviceSettings";
import { renderPollForTentativeDevicePage } from "./flows/addDevice/manage/pollForTentativeDevice";
import {
  registerTentativeDevice,
  TentativeDeviceInfo,
} from "./flows/addDevice/welcomeView/registerTentativeDevice";
import { deviceRegistrationDisabledInfo } from "./flows/addDevice/welcomeView/deviceRegistrationModeDisabled";
import { showVerificationCodePage } from "./flows/addDevice/welcomeView/showVerificationCode";
import { verifyTentativeDevicePage } from "./flows/addDevice/manage/verifyTentativeDevice";
import { mkAnchorPicker } from "./components/anchorPicker";
import { withLoader } from "./components/loader";
import { displaySafariWarning } from "./flows/recovery/displaySafariWarning";
import { displayError } from "./components/displayError";
import { promptUserNumber } from "./components/promptUserNumber";
import { registerDisabled } from "./flows/registerDisabled";
import { RecoveryDevice } from "./utils/recoveryDevice";

// A "dummy" connection which actually is just undefined, hoping pages won't call it
const dummyConnection = undefined as unknown as AuthenticatedConnection;
const userNumber = BigInt(10000);

const i18n = new I18n("en");

const recoveryPhrase: RecoveryDevice & DeviceData = {
  alias: "Recovery Phrase",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { seed_phrase: null },
  purpose: { recovery: null },
  credential_id: [],
  origin: [],
};

const recoveryPhraseText =
  "10050 mandate vague same suspect eight pet gentle repeat maple actor about legal sword text food print material churn perfect sword blossom sleep vintage blouse";

const recoveryDevice: RecoveryDevice & DeviceData = {
  alias: "Recovery Device",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { unknown: null },
  purpose: { recovery: null },
  credential_id: [],
  origin: [],
};

const simpleDevices: [DeviceData, DeviceData] = [
  {
    alias: "Chrome on iPhone",
    protection: { unprotected: null },
    pubkey: [1, 2, 3, 4],
    key_type: { unknown: null },
    purpose: { authentication: null },
    credential_id: [],
    origin: [],
  },

  {
    alias: "Yubikey Blue",
    protection: { unprotected: null },
    pubkey: [1, 2, 3, 5],
    key_type: { unknown: null },
    purpose: { authentication: null },
    credential_id: [],
    origin: [],
  },
];

const defaultPage = () => {
  document.title = "Showcase";
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
};

// A challenge with a base64 CAPTCHA, apologies for the length
const dummyChallenge: Challenge = {
  png_base64:
    "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",
  challenge_key: "unimportant",
};

/* Various values used for showcasing both authz & manage authentication flows */

const authnCnfg = {
  register: () => console.log("Register requested"),
  addDevice: () => console.log("Add device requested"),
  recover: () => console.log("Recover requested"),
  onSubmit: (anchor: bigint) => console.log("Submitting anchor", anchor),
};

const authzTemplates = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  i18n,
});
const authzTemplatesAlt = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  derivationOrigin: "http://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app",
  i18n,
});

const authz = authnPages({ ...authnCnfg, ...authzTemplates });
const authzAlt = authnPages({ ...authnCnfg, ...authzTemplatesAlt });

const manageTemplates = authnTemplateManage();
const manage = authnPages({ ...authnCnfg, ...manageTemplates });

const iiPages: Record<string, () => void> = {
  displayUserNumber: () =>
    displayUserNumberPage({
      userNumber,
      onContinue: () => console.log("done"),
    }),
  about: () => aboutView(),
  compatibilityNotice: () => compatibilityNotice("This is the reason."),
  pickRecoveryDevice: () =>
    pickRecoveryDevice([recoveryPhrase, recoveryDevice]),
  promptDeviceAlias: () =>
    promptDeviceAliasPage({
      cancel: () => console.log("canceled"),
      continue: (alias) => console.log("device alias:", alias),
      i18n,
    }),
  pickDeviceAlias: () =>
    pickDeviceAliasPage({
      cancel: () => console.log("canceled"),
      pick: (alias) => console.log("device alias:", alias),
    }),

  // Authorize screens

  authorizeNew: () =>
    authz.firstTime({ useExisting: () => console.log("Use existing") }),
  authorizeUseExisting: () => authz.useExisting(),
  authorizePick: () =>
    authz.pick({
      anchors: [BigInt(10000), BigInt(243099)],
      moreOptions: () => console.log("More options requested"),
    }),
  authorizePickAlt: () =>
    authzAlt.pick({
      anchors: [BigInt(10000), BigInt(243099)],
      moreOptions: () => console.log("More options requested"),
    }),
  authorizePickAltOpen: () =>
    authzAlt.pick({
      anchors: [BigInt(10000), BigInt(243099)],
      moreOptions: () => console.log("More options requested"),
    }),
  authorizePickMany: () =>
    authz.pick({
      anchors: [...Array(10).keys()].map((x) =>
        BigInt(10000 + 129 * x * x)
      ) as NonEmptyArray<bigint>,
      moreOptions: () => console.log("More options requested"),
    }),

  // Manage Auth screens
  manageNew: () =>
    manage.firstTime({ useExisting: () => console.log("Use existing") }),
  manageUseExisting: () => manage.useExisting(),
  managePick: () =>
    manage.pick({
      anchors: [BigInt(10000), BigInt(243099)],
      moreOptions: () => console.log("More options requested"),
    }),

  recoverWithPhrase: () =>
    phraseRecoveryPage(userNumber, dummyConnection, recoveryPhrase),
  recoverWithDevice: () =>
    deviceRecoveryPage(userNumber, dummyConnection, recoveryDevice),
  constructing: () => renderConstructing(),
  promptCaptcha: () =>
    promptCaptchaPage({
      cancel: () => console.log("canceled"),
      requestChallenge: () =>
        new Promise(() => {
          /* noop */
        }),
      verifyChallengeChars: () =>
        new Promise(() => {
          /* noop */
        }),
      onContinue: () => console.log("Done"),
      i18n,
    }),
  promptCaptchaReady: () =>
    promptCaptchaPage({
      cancel: () => console.log("canceled"),
      requestChallenge: () => Promise.resolve(dummyChallenge),
      verifyChallengeChars: () =>
        new Promise(() => {
          /* noop */
        }),
      onContinue: () => console.log("Done"),
      i18n,
    }),
  chooseRecoveryMechanism: () =>
    chooseRecoveryMechanismPage({
      disablePhrase: true,
      disableKey: false,
      pick: (recovery) => console.log("picked:", recovery),
      cancel: () => console.log("canceled"),
      title: html`Choose a Recovery Method`,
      message: html`We recommend that you create at least one recovery method in
      case you lose access to your devices.`,
      cancelText: html`Skip, I understand the risks`,
    }),
  displaySingleDeviceWarning: () =>
    displaySingleDeviceWarning(
      userNumber,
      dummyConnection,
      (_anchor, _conn) => {
        return Promise.resolve();
      }
    ),
  displayManage: () =>
    displayManagePage({
      userNumber,
      authenticators: [
        {
          label: "Chrome on iPhone",
          isRecovery: false,
          openSettings: () => Promise.resolve(),
          settings: [
            {
              label: "Remove",
              fn: () => Promise.resolve(),
            },
          ],
        },
        {
          label: "Yubikey Blue",
          isRecovery: false,
          openSettings: () => Promise.resolve(),
          settings: [
            {
              label: "Remove",
              fn: () => Promise.resolve(),
            },
          ],
        },
        {
          label: "Yubikey Blue",
          isRecovery: false,
          openSettings: () => Promise.resolve(),
          warn: html`Something is rotten in the state of Device`,
        },
      ],
      recoveries: [
        {
          label: "Recovery Phrase",
          isRecovery: true,
          openSettings: () => Promise.resolve(),
          settings: [
            {
              label: "Remove",
              fn: () => Promise.resolve(),
            },
            {
              label: "Protect",
              fn: () => Promise.resolve(),
            },
          ],
        },
      ],
      onAddDevice: () => {
        console.log("add device requested");
      },
      onAddRecovery: () => {
        console.log("add recovery requested");
      },
    }),
  displayManageSingle: () =>
    displayManagePage({
      userNumber,
      authenticators: [
        {
          label: "Chrome on iPhone",
          isRecovery: false,
          openSettings: () => Promise.resolve(),
        },
      ],
      recoveries: [],
      onAddDevice: () => {
        console.log("add device requested");
      },
      onAddRecovery: () => {
        console.log("add recovery requested");
      },
    }),
  chooseDeviceAddFlow: () => chooseDeviceAddFlow(),
  renderPollForTentativeDevicePage: () =>
    renderPollForTentativeDevicePage(userNumber),
  registerTentativeDevice: () =>
    registerTentativeDevice(userNumber, dummyConnection),
  deviceRegistrationDisabledInfo: () =>
    deviceRegistrationDisabledInfo(dummyConnection, [
      userNumber,
    ] as unknown as TentativeDeviceInfo),
  showVerificationCode: () =>
    showVerificationCodePage({
      alias: simpleDevices[0].alias,
      tentativeRegistrationInfo: {
        verification_code: "123456",
        device_registration_timeout: undefined as unknown as Timestamp,
      },
      cancel: () => console.log("canceled"),
      remaining: {
        async *[Symbol.asyncIterator]() {
          yield "00:34";
        },
      },
    }),
  verifyTentativeDevice: () =>
    verifyTentativeDevicePage({
      alias: simpleDevices[0].alias,
      cancel: () => console.log("canceled"),
      verify: () => Promise.resolve({ retry: null }),
      doContinue: (v) => console.log("continue with:", v),
      remaining: {
        async *[Symbol.asyncIterator]() {
          yield "00:34";
        },
      },
    }),
  deviceSettings: () =>
    deviceSettingsPage({
      device: simpleDevices[0],
      isOnlyDevice: false,
      protectDevice: () => console.log("protect"),
      deleteDevice: () => console.log("delete"),
      back: () => console.log("back"),
    }),
  loader: () => withLoader(() => new Promise(() => renderConstructing())),
  displaySafariWarning: () =>
    displaySafariWarning(userNumber, dummyConnection, (_anchor, _conn) => {
      return Promise.resolve();
    }),
  displaySeedPhrase: () => displaySeedPhrase(recoveryPhraseText),
  displayError: () =>
    displayError({
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: "oh my, so much to say. SO MUCH!",
      primaryButton: "Try again",
    }),
  promptUserNumber: () => promptUserNumber({ title: "hello" }),
  banner: () => {
    manage.firstTime({ useExisting: () => console.log("Use existing") });
    showWarning(html`This is a test page, be very careful!`);
  },
  registerDisabled: () => registerDisabled(),
};

const showcase: TemplateResult = html`
  <h1 class="t-title t-title--main">showcase</h1>
  <div class="showcase-grid l-stack">
    ${Object.entries(iiPages).map(([key, _]) => {
      return html`<aside>
        <a data-page-name="${key}" href="${key}">
          <iframe src="${key}" title="${key}"></iframe>
          <h2>${key}</h2>
        </a>
      </aside>`;
    })}
  </div>
`;

const components = (): TemplateResult => {
  const showSelected: Ref<HTMLDivElement> = createRef();
  const savedAnchors: Ref<HTMLInputElement> = createRef();
  const updateSavedAnchors: Ref<HTMLButtonElement> = createRef();

  const mk = (anchors: NonEmptyArray<bigint>): TemplateResult =>
    mkAnchorPicker({
      savedAnchors: anchors,
      pick: (anchor: bigint) =>
        withRef(showSelected, (div) => {
          div.innerText = anchor.toString();
        }),
      moreOptions: () => console.log("More options requested"),
    }).template;

  const chan = new Chan<TemplateResult>();

  const update = () => {
    const value = savedAnchors.value?.value;
    if (value !== undefined) {
      if (value !== "") {
        const values = value.split(",").map((x) => BigInt(x));
        const anchors = asNonEmptyArray(values);
        if (anchors !== undefined) {
          chan.send(mk(anchors));
        }
      }
    }
  };

  chan.send(mk([BigInt(10055), BigInt(1669234)]));

  return html`
    <div class="c-card" style="margin: 40px;">
        <input class="c-input" ${ref(
          savedAnchors
        )} placeholder="stored anchors: anchor1, anchor2, ..." ></input>
        <button class="c-button" ${ref(
          updateSavedAnchors
        )} @click="${update}">update</button>
        <div>${asyncReplace(chan.recv())}</div>
    <div ${ref(
      showSelected
    )} class="c-input c-input--readonly">Please select anchor</div></div>
    <div
        ${mount((container) =>
          container instanceof HTMLElement
            ? promptCaptchaPage(
                {
                  cancel: () => console.log("canceled"),
                  requestChallenge: () =>
                    new Promise((resolve) => setTimeout(resolve, 1000)).then(
                      () => dummyChallenge
                    ),
                  verifyChallengeChars: (cr) =>
                    new Promise((resolve) => setTimeout(resolve, 1000)).then(
                      () => (cr.chars === "8wJ6Q" ? "yes" : badChallenge)
                    ),
                  onContinue: () => console.log("Done"),
                  i18n,
                },
                container
              )
            : ""
        )}>
                </div>

    `;
};

const i18nExample = () => {
  type Lang = "en" | "fr";
  const exampleI18n = new I18n<Lang>("en");

  const copy = exampleI18n.i18n({
    en: {
      title: "i18n support",
      paragraph:
        "This is an example of internationalization support in Internet Identity. Click a button to change the language.",
    },
    fr: {
      title: "support i18n",

      paragraph:
        "Ceci est un exemple de support multi-language dans Internet Identity. Cliquez un des boutons ci-dessous pour changer la langue.",
    },
  });

  const langIs = (lang: Lang) =>
    asyncReplace(exampleI18n.getLanguageAsync(), (x) => x == lang);
  const langButton = (lang: Lang) => html`
    <button
      ?disabled=${langIs(lang)}
      class="c-button"
      @click=${() => exampleI18n.setLanguage(lang)}
    >
      ${lang}
    </button>
  `;

  return html`
    <style>
      .i18n-example {
        margin: 10rem auto;
        max-width: 60rem;
        padding: 0 2rem;
      }
    </style>
    <section class="i18n-example">
      <article class="l-statck c-card c-card--highlight">
        <h2 class="t-title t-tile--main">${copy.title}</h2>
        <p class="t-lead">${copy.paragraph}</p>
        <div class="c-button-group">
          ${langButton("en")} ${langButton("fr")}
        </div>
      </article>
    </section>
  `;
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
  ${showcase} ${i18nExample()} ${components()} ${styleguide}
`;

const init = async () => {
  // We use the URL's path to route to the correct page.
  // If we can't find a page to route to, we just show the default page.
  // This is not very user friendly (in particular we don't show anything like a
  // 404) but this is an dev page anyway.
  const route = window.location.pathname
    .replace(process.env.BASE_URL ?? "", "")
    .substring(1);
  const page = iiPages[route] ?? defaultPage;

  page();
};

init();
