/** A showcase for static pages. II pages are given a fake connection and loaded from here
 * just to give an idea of what they look like, and to speed up the development cycle when
 * working on HTML and CSS. */
import {
  Challenge,
  DeviceData,
  Timestamp,
} from "$generated/internet_identity_types";
import { showWarning } from "$src/banner";
import { promptDeviceAliasPage } from "$src/components/alias";
import { mkAnchorPicker } from "$src/components/anchorPicker";
import { authnPages } from "$src/components/authenticateBox";
import { displayError } from "$src/components/displayError";
import { loadIdentityBackground } from "$src/components/identityCard";
import { withLoader } from "$src/components/loader";
import { showMessage, showMessagePage } from "$src/components/message";
import { promptUserNumber } from "$src/components/promptUserNumber";
import { showSpinnerPage } from "$src/components/spinner";
import { toast } from "$src/components/toast";
import { addDeviceSuccessPage } from "$src/flows/addDevice/manage/addDeviceSuccess";
import { pollForTentativeDevicePage } from "$src/flows/addDevice/manage/pollForTentativeDevice";
import { verifyTentativeDevicePage } from "$src/flows/addDevice/manage/verifyTentativeDevice";
import { deviceRegistrationDisabledInfoPage } from "$src/flows/addDevice/welcomeView/deviceRegistrationModeDisabled";
import { showVerificationCodePage } from "$src/flows/addDevice/welcomeView/showVerificationCode";
import { authnTemplateAuthorize } from "$src/flows/authorize";
import { compatibilityNotice } from "$src/flows/compatibilityNotice";
import { dappsExplorerPage } from "$src/flows/dappsExplorer";
import { getDapps } from "$src/flows/dappsExplorer/dapps";
import { authnTemplateManage, displayManagePage } from "$src/flows/manage";
import {
  protectDeviceInfoPage,
  resetPhraseInfoPage,
  unprotectDeviceInfoPage,
} from "$src/flows/manage/deviceSettings";
import {
  checkIndices,
  confirmSeedPhrasePage,
} from "$src/flows/recovery/confirmSeedPhrase";
import { displaySeedPhrasePage } from "$src/flows/recovery/displaySeedPhrase";
import { pickRecoveryDevice } from "$src/flows/recovery/pickRecoveryDevice";
import { deviceRecoveryPage } from "$src/flows/recovery/recoverWith/device";
import { recoverWithPhrasePage } from "$src/flows/recovery/recoverWith/phrase";
import { addPhrasePage } from "$src/flows/recovery/recoveryWizard";
import { badChallenge, promptCaptchaPage } from "$src/flows/register/captcha";
import { displayUserNumberPage } from "$src/flows/register/finish";
import { savePasskeyPage } from "$src/flows/register/passkey";
import { registerDisabled } from "$src/flows/registerDisabled";
import { styleguide } from "$src/styleguide";
import "$src/styles/main.css";
import { I18n } from "$src/utils/i18n";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { mount, withRef } from "$src/utils/lit-html";
import { RecoveryDevice } from "$src/utils/recoveryDevice";
import { asNonEmptyArray, Chan, NonEmptyArray } from "$src/utils/utils";
import { html, render, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

const identityBackground = loadIdentityBackground();

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
  metadata: [],
};

const recoveryPhraseText =
  "10050 mandate vague same suspect eight pet gentle repeat maple actor about legal sword text food print material churn perfect sword blossom sleep vintage blouse";

const recoveryAnchorWord = recoveryPhraseText.split(" ")[0];
const recoveryWords = recoveryPhraseText.split(" ").slice(1);

const recoveryDevice: RecoveryDevice & DeviceData = {
  alias: "Recovery Device",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { unknown: null },
  purpose: { recovery: null },
  credential_id: [],
  origin: [],
  metadata: [],
};

const chromeDevice: DeviceData = {
  alias: "Chrome on iPhone",
  protection: { unprotected: null },
  pubkey: [1, 2, 3, 4],
  key_type: { unknown: null },
  purpose: { authentication: null },
  credential_id: [],
  origin: [],
  metadata: [],
};
export const defaultPage = () => {
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

const dapps = getDapps();

const authzTemplates = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  i18n,
});
const authzTemplatesAlt = authnTemplateAuthorize({
  origin: "https://nowhere.com",
  derivationOrigin: "http://fgte5-ciaaa-aaaad-aaatq-cai.ic0.app",
  i18n,
});

const authzTemplatesKnownAlt = authnTemplateAuthorize({
  origin: "https://nns.ic0.app",
  derivationOrigin: "http://fgte8-ciaaa-aaaad-aaatq-cai.ic0.app",
  i18n,

  knownDapp: dapps.find((dapp) => dapp.name === "NNS Dapp"),
});

const authzTemplatesKnown = authnTemplateAuthorize({
  origin: "https://oc.app",
  i18n,
  knownDapp: dapps.find((dapp) => dapp.name === "OpenChat"),
});

const authz = authnPages(i18n, { ...authnCnfg, ...authzTemplates });
const authzAlt = authnPages(i18n, { ...authnCnfg, ...authzTemplatesAlt });
const authzKnown = authnPages(i18n, { ...authnCnfg, ...authzTemplatesKnown });
const authzKnownAlt = authnPages(i18n, {
  ...authnCnfg,
  ...authzTemplatesKnownAlt,
});

const manageTemplates = authnTemplateManage({ dapps });
const manage = authnPages(i18n, { ...authnCnfg, ...manageTemplates });

export const iiPages: Record<string, () => void> = {
  displayUserNumber: () =>
    displayUserNumberPage({
      identityBackground,
      userNumber,
      onContinue: () => console.log("done"),
    }),
  compatibilityNotice: () => compatibilityNotice("This is the reason."),
  pickRecoveryDevice: () =>
    pickRecoveryDevice([recoveryPhrase, recoveryDevice]),
  promptDeviceAlias: () =>
    promptDeviceAliasPage({
      title: "Register this device",
      cancel: () => console.log("canceled"),
      continue: (alias) => console.log("device alias:", alias),
      i18n,
    }),

  // Authorize screens

  authorizeNew: () =>
    authz.firstTime({ useExisting: () => console.log("Use existing") }),
  authorizeNewKnown: () =>
    authzKnown.firstTime({ useExisting: () => console.log("Use existing") }),
  authorizeNewAlt: () =>
    authzAlt.firstTime({ useExisting: () => console.log("Use existing") }),
  authorizeNewKnownAlt: () =>
    authzKnownAlt.firstTime({ useExisting: () => console.log("Use existing") }),
  authorizeUseExisting: () => authz.useExisting(),
  authorizeUseExistingKnown: () => authzKnown.useExisting(),
  authorizeUseExistingKnownAlt: () => authzKnownAlt.useExisting(),
  authorizePick: () =>
    authz.pick({
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

  protectDeviceInfo: () =>
    protectDeviceInfoPage({
      next: () => console.log("next"),
      cancel: () => console.log("cancel"),
      i18n,
    }),

  unprotectDeviceInfo: () =>
    unprotectDeviceInfoPage({
      next: () => console.log("next"),
      cancel: () => console.log("cancel"),
      i18n,
    }),

  resetPhraseInfo: () =>
    resetPhraseInfoPage({
      next: () => console.log("next"),
      cancel: () => console.log("cancel"),
      i18n,
    }),

  recoverWithPhrase: () =>
    recoverWithPhrasePage<
      { tag: "ok"; words: string },
      { tag: "err"; message: string }
    >({
      verify: (words: string) => Promise.resolve({ tag: "ok", words }),
      confirm: ({ words }) => console.log("confirmed: " + words),
      back: () => console.log("remove"),
      message: "Something cool will happen if you type your anchor",
    }),
  recoverWithDevice: () =>
    deviceRecoveryPage(userNumber, dummyConnection, recoveryDevice),
  savePasskey: () =>
    savePasskeyPage({
      i18n,
      cancel: () => console.log("cancel"),
      construct: () =>
        new Promise((_) => {
          console.log("Identity Construction");
        }),
    }),
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
  displayManage: () => {
    displayManagePage({
      identityBackground,
      userNumber,
      devices: {
        authenticators: [
          {
            alias: "Chrome on iPhone",
            remove: () => console.log("remove"),
            rename: () => console.log("rename"),
          },
          {
            alias: "Yubikey Blue",
            remove: () => console.log("remove"),
            rename: () => console.log("rename"),
          },
          {
            alias: "Yubikey Blue",
            remove: () => console.log("remove"),
            rename: () => console.log("rename"),
            warn: html`Something is rotten in the state of Device`,
          },
        ],
        recoveries: {
          recoveryPhrase: {
            isProtected: true,
            unprotect: () => console.log("unprotect"),
            reset: () => Promise.resolve(),
          },
        },
      },
      onAddDevice: () => {
        console.log("add device requested");
      },
      addRecoveryPhrase: () => {
        console.log("add recovery phrase");
      },
      addRecoveryKey: () => {
        console.log("add recovery key");
      },
      dapps,
      exploreDapps: () => {
        console.log("explore dapps");
      },
    });
  },
  displayManageSingle: () => {
    displayManagePage({
      identityBackground,
      userNumber,
      devices: {
        authenticators: [
          {
            alias: "Chrome on iPhone",
            rename: () => console.log("rename"),
          },
        ],
        recoveries: {},
      },
      onAddDevice: () => {
        console.log("add device requested");
      },
      addRecoveryPhrase: () => {
        console.log("add recovery phrase");
      },
      addRecoveryKey: () => {
        console.log("add recovery key");
      },
      dapps,
      exploreDapps: () => {
        console.log("explore dapps");
      },
    });
  },
  pollForTentativeDevicePage: () =>
    pollForTentativeDevicePage({
      cancel: () => console.log("canceled"),
      useFIDO: () => console.log("use FIDO"),
      origin: "https://identity.internetcomputer.org",
      userNumber: BigInt(1234),
      remaining: {
        async *[Symbol.asyncIterator]() {
          yield "00:34";
        },
      },
      i18n,
    }),
  deviceRegistrationDisabledInfo: () =>
    deviceRegistrationDisabledInfoPage({
      userNumber,
      retry: () => console.log("retry"),
      cancel: () => console.log("canceled"),
    }),
  showVerificationCode: () =>
    showVerificationCodePage({
      alias: chromeDevice.alias,
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
      alias: chromeDevice.alias,
      cancel: () => console.log("canceled"),
      verify: () => Promise.resolve({ retry: null }),
      doContinue: (v) => console.log("continue with:", v),
      remaining: {
        async *[Symbol.asyncIterator]() {
          yield "00:34";
        },
      },
    }),
  loader: () =>
    withLoader(() => new Promise(() => showMessage({ message: "Loading..." }))),
  addPhrase: () =>
    addPhrasePage({
      ok: () => console.log("ok"),
      cancel: () => console.log("cancel"),
      i18n,
      intent: "userInitiated",
    }),
  addPhraseWarning: () =>
    addPhrasePage({
      ok: () => console.log("ok"),
      cancel: () => console.log("cancel"),
      i18n,
      intent: "securityReminder",
    }),
  displaySeedPhrase: () =>
    displaySeedPhrasePage({
      operation: "create",
      userNumberWord: recoveryAnchorWord,
      words: recoveryWords,
      cancel: () => console.log("cancel"),
      onContinue: () => console.log("continue with:"),
      copyPhrase: () => Promise.resolve(console.log("copied")),
      i18n,
    }),
  confirmSeedPhrase: () =>
    confirmSeedPhrasePage({
      confirm: () => console.log("confirmed"),
      back: () => console.log("back"),
      userNumberWord: recoveryAnchorWord,
      words: recoveryWords.map((word, i) => ({
        word,
        check: checkIndices.includes(i),
      })),
      i18n,
    }),
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
  toasts: () => {
    showMessage({ message: html`hello` });
    toast.error("This is an error!");
    toast.error(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec varius tellus id massa lobortis, et luctus nulla consequat. Phasellus lacinia velit non quam placerat imperdiet. In elementum orci sit amet malesuada eleifend. Vestibulum ultricies fringilla lorem sit amet laoreet. Suspendisse aliquet tincidunt risus, sed pellentesque purus porttitor nec."
    );
  },
  dappsExplorer: () => {
    dappsExplorerPage({ dapps, i18n, back: () => console.log("back") });
  },
  showMessage: () =>
    showMessagePage({
      message: "You may close this page.",
    }),
  showSpinner: () =>
    showSpinnerPage({
      message: "Good things come to those who wait.",
    }),
  addDeviceSuccess: () =>
    addDeviceSuccessPage({
      i18n,
      deviceAlias: chromeDevice.alias,
      onContinue: () => console.log("Continue"),
    }),
};

const showcase: TemplateResult = html`
  <h1 class="t-title t-title--main">showcase</h1>
  <div class="showcase-grid l-stack">
    ${Object.entries(iiPages).map(([pageName, _]) => {
      // '/' or '/internet-identity/'
      const baseUrl = import.meta.env.BASE_URL ?? "/";
      // '/myPage' or '/internet-identity/myPage'
      const pageLink = baseUrl + pageName;
      return html`<aside>
        <a data-page-name=${pageName} href=${pageLink}>
          <iframe src=${pageLink} title=${pageName}></iframe>
          <h2>${pageName}</h2>
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
      focus: false,
    }).template;

  const chan = new Chan<TemplateResult>(html`loading...`);

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
        <div>${asyncReplace(chan)}</div>
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
                  focus: false,
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

// The 404 page
export const notFound = () => {
  showMessagePage({
    message: html`<h1>404 not found</h1>
      <p class="t-paragraph">Page was not found</p> `,
  });
};
