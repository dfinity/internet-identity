import { registerTentativeDevice } from "$src/flows/addDevice/welcomeView/registerTentativeDevice";
import { useRecovery } from "$src/flows/recovery/useRecovery";
import { I18n } from "$src/i18n";
import {
  apiResultToLoginFlowResult,
  LoginData,
  LoginFlowError,
  LoginFlowResult,
  LoginFlowSuccess,
} from "$src/utils/flowResult";
import { Connection } from "$src/utils/iiConnection";
import { TemplateElement, withRef } from "$src/utils/lit-html";
import { registerIfAllowed } from "$src/utils/registerAllowedCheck";
import {
  getAnchors,
  parseUserNumber,
  setAnchorUsed,
} from "$src/utils/userNumber";
import { isNonEmptyArray, NonEmptyArray, unreachable } from "$src/utils/utils";
import { nonNullish } from "@dfinity/utils";
import { html, render, TemplateResult } from "lit-html";
import { mkAnchorInput } from "./anchorInput";
import { mkAnchorPicker } from "./anchorPicker";
import { displayError } from "./displayError";
import {
  controlIcon,
  githubBigIcon,
  participateIcon,
  privacyIcon,
  secureIcon,
  signInIcon,
} from "./icons";
import { withLoader } from "./loader";
import { mainWindow } from "./mainWindow";
import { promptUserNumber } from "./promptUserNumber";

import { getDapps } from "$src/flows/dappsExplorer/dapps";
import { dappsHeader } from "$src/flows/dappsExplorer/teaser";
import { shuffleArray } from "$src/utils/utils";

import copyJson from "./authenticateBox.json";

/** dapps visual used in sidebar */
const dappsVisual = (): TemplateResult => {
  const dapps = shuffleArray(getDapps());
  return dappsHeader({
    dapps,
    clickable: false,
  });
};

/** Template used for rendering specific authentication screens. See `authnPages` below
 * for meaning of "firstTime", "useExisting" and "pick". */
export type AuthnTemplates = {
  firstTime: {
    slot: TemplateResult;
    useExistingText: TemplateElement /** text shown on the button leading to "useExisting" */;
    createAnchorText: TemplateElement /** text shown on the button leading to "useExisting" */;
  };
  useExisting: {
    slot: TemplateResult;
  };
  pick: {
    slot: TemplateResult;
  };
};

/** Authentication box component which authenticates a user
 * to II or to another dapp */
export const authenticateBox = async (
  connection: Connection,
  i18n: I18n,
  templates: AuthnTemplates
): Promise<LoginData & { newAnchor: boolean }> => {
  const promptAuth = () =>
    new Promise<LoginFlowResult & { newAnchor: boolean }>((resolve) => {
      const pages = authnPages(i18n, {
        ...templates,
        addDevice: (userNumber) => asNewDevice(connection, userNumber),
        onSubmit: async (userNumber) => {
          resolve({
            newAnchor: false,
            ...(await authenticate(connection, userNumber)),
          });
        },
        register: async () => {
          resolve({
            ...(await registerIfAllowed(connection)),
            newAnchor: true,
          });
        },
        recover: async (userNumber) => {
          resolve({
            ...(await useRecovery(connection, userNumber)),
            newAnchor: false,
          });
        },
      });

      // If there _are_ some anchors, then we show the "pick" screen, otherwise
      // we assume a new user and show the "firstTime" screen.
      const anchors = getAnchors();
      if (isNonEmptyArray(anchors)) {
        pages.pick({
          anchors: anchors,
          moreOptions: () => pages.useExisting(),
        });
      } else {
        pages.firstTime({ useExisting: () => pages.useExisting() });
      }
    });

  // Retry until user has successfully authenticated
  for (;;) {
    const { newAnchor, ...result } = await promptAuth();
    const loginData = await handleLoginFlowResult(result);

    if (nonNullish(loginData)) {
      return { ...loginData, newAnchor };
    }
  }
};

export const handleLoginFlowResult = async (
  result: LoginFlowResult
): Promise<LoginData | undefined> => {
  switch (result.tag) {
    case "ok":
      setAnchorUsed(result.userNumber);
      return result;
    case "err":
      await displayError({
        title: result.title,
        message: result.message,
        detail: result.detail !== "" ? result.detail : undefined,
        primaryButton: "Try again",
      });
      break;
    case "canceled":
      break;
    default:
      unreachable(result);
      break;
  }

  return undefined;
};

/** The templates for the authentication pages */
export const authnTemplates = (
  i18n: I18n,
  props: {
    register: () => void;
    onSubmit: (anchor: bigint) => void;
    addDevice: (anchor?: bigint) => void;
    recover: (anchor?: bigint) => void;
  } & AuthnTemplates
) => {
  const copy = i18n.i18n(copyJson);

  const marketingBlocks = [
    {
      icon: secureIcon,
      title: copy.secure_and_convenient,
      body: copy.instead_of_passwords,
    },
    {
      icon: privacyIcon,
      title: copy.no_tracking,
      body: copy.get_privacy,
    },
    {
      icon: controlIcon,
      title: copy.control_your_identity,
      body: copy.securely_access,
    },
    {
      icon: participateIcon,
      title: copy.own_and_participate,
      body: copy.share_and_vote,
    },
    {
      icon: signInIcon,
      title: copy.sign_in_to_web3,
      body: copy.manages_keys,
    },
    {
      icon: githubBigIcon,
      title: copy.opensource_and_transparent,
      body: copy.internet_identity_codebase,
    },
  ];

  return {
    firstTime: (firstTimeProps: { useExisting: () => void }) => {
      return html`${props.firstTime.slot}
        <div class="l-stack">
          <button
            type="button"
            @click=${() => props.register()}
            id="registerButton"
            class="c-button"
          >
            ${props.firstTime.createAnchorText}
          </button>
          <button
            type="button"
            @click=${() => firstTimeProps.useExisting()}
            id="loginButton"
            class="c-button c-button--secondary"
          >
            ${props.firstTime.useExistingText}
          </button>
        </div>
        <div class="c-marketing-section">
          ${marketingBlocks.map(
            ({ title, body, icon }) =>
              html`
                <article class="c-marketing-block">
                  ${icon !== undefined
                    ? html`<i class="c-icon c-icon--marketing">${icon}</i>`
                    : undefined}
                  <h2 class="t-title t-title--main">${title}</h2>
                  <p class="t-paragraph t-paragraph--weak">${body}</p>
                </article>
              `
          )}
        </div> `;
    },
    useExisting: () => {
      const anchorInput = mkAnchorInput({ onSubmit: props.onSubmit });
      const withUserNumber = (f: (arg: bigint | undefined) => void) => {
        const value = withRef(
          anchorInput.userNumberInput,
          (input) => input.value
        );

        // XXX: we work around parseUserNumber returning "null" by defaulting to "undefined"
        const userNumber = parseUserNumber(value ?? "") ?? undefined;
        f(userNumber);
      };
      return html` ${props.useExisting.slot} ${anchorInput.template}
        <ul class="c-list--flex l-stack--tight">
          <li>
            <a
              @click=${() =>
                withUserNumber((userNumber) => props.addDevice(userNumber))}
              id="addNewDeviceButton"
              class="t-link"
              >Add a new Passkey?</a
            >
          </li>
          <li>
            <a
              @click="${() =>
                withUserNumber((userNumber) => props.recover(userNumber))}"
              id="recoverButton"
              class="t-link"
              >Lost Access?</a
            >
          </li>
        </ul>
        <div class="l-stack c-button-group">
          <button
            @click=${() => props.register()}
            class="c-button c-button--secondary"
          >
            Create New
          </button>
          <button
            data-action="continue"
            @click=${() => anchorInput.submit()}
            class="c-button"
          >
            Continue
          </button>
        </div>`;
    },
    pick: (pickProps: {
      anchors: NonEmptyArray<bigint>;
      moreOptions: () => void;
    }) => {
      return html`
        ${props.pick.slot}
        ${mkAnchorPicker({
          savedAnchors: pickProps.anchors,
          pick: props.onSubmit,
          moreOptions: pickProps.moreOptions,
        }).template}
      `;
    },
  };
};

/** The authentication pages, namely "firstTime" (for new users), "useExisting" (for users who
 * don't have saved anchors or who wish to use non-saved anchors) and "pick" (for users
 * picking a saved anchor) */
export const authnPages = (
  i18n: I18n,
  props: {
    register: () => void;
    onSubmit: (anchor: bigint) => void;
    addDevice: (anchor?: bigint) => void;
    recover: (anchor?: bigint) => void;
  } & AuthnTemplates
) => {
  const tpls = authnTemplates(i18n, props);
  return {
    firstTime: (firstTimeProps: { useExisting: () => void }) =>
      page(tpls.firstTime(firstTimeProps)),
    useExisting: () => page(tpls.useExisting()),
    pick: (pickProps: {
      anchors: NonEmptyArray<bigint>;
      moreOptions: () => void;
    }) => page(tpls.pick(pickProps)),
  };
};

export const authenticate = async (
  connection: Connection,
  userNumber: bigint
): Promise<LoginFlowSuccess | LoginFlowError> => {
  try {
    const result = await withLoader(() => connection.login(userNumber));
    return apiResultToLoginFlowResult(result);
  } catch (error) {
    return {
      tag: "err",
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: error instanceof Error ? error.message : JSON.stringify(error),
    };
  }
};

// Wrap the template with header & footer and render the page
// we are including the logo because of SVG ID conflicts, instead of using the 
// logo from the main window
const page = (slot: TemplateResult) => {
  const template = mainWindow({
    slot: html` <!-- The title is hidden but used for accessibility -->
      <h1 data-page="authenticate" class="is-hidden">Internet Identity</h1>
      ${slot}`,
    slotSidebar: html`<div class="l-sidebar is-hidden--mobile">
      <div class="l-sidebar__main">
        <div class="c-logo c-logo--sidebar">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 233 111"
          >
            <defs>
              <linearGradient
                id="grad-o-y-sidebar"
                x1="145.304"
                x2="221.385"
                y1="7.174"
                y2="85.958"
                gradientUnits="userSpaceOnUse"
              >
                <stop offset=".21" stop-color="#F15A24" />
                <stop offset=".684" stop-color="#FBB03B" />
              </linearGradient>
              <linearGradient
                id="grad-p-p-sidebar"
                x1="85.087"
                x2="9.006"
                y1="101.622"
                y2="22.838"
                gradientUnits="userSpaceOnUse"
              >
                <stop offset=".21" stop-color="#ED1E79" />
                <stop offset=".893" stop-color="#522785" />
              </linearGradient>
            </defs>
            <g transform="translate(0 2)">
              <path
                fill="url(#grad-o-y-sidebar)"
                d="M174.433 0c-12.879 0-26.919 6.6-41.758 19.6-7.04 6.159-13.12 12.759-17.679 18.038l.04.04v-.04s7.199 7.84 15.159 16.24c4.28-5.08 10.44-12 17.519-18.24 13.2-11.559 21.799-13.999 26.719-13.999 18.52 0 33.559 14.68 33.559 32.719 0 17.92-15.079 32.599-33.559 32.719-.84 0-1.92-.12-3.28-.4 5.4 2.32 11.2 4 16.72 4 33.918 0 40.558-22.12 40.998-23.72 1-4.04 1.52-8.28 1.52-12.64C230.391 24.4 205.272 0 174.433 0Z"
              />
              <path
                fill="url(#grad-p-p-sidebar)"
                d="M55.958 108.796c12.88 0 26.919-6.6 41.758-19.6 7.04-6.16 13.12-12.759 17.679-18.039l-.04-.04v.04s-7.199-7.84-15.159-16.24c-4.28 5.08-10.44 12-17.52 18.24-13.199 11.56-21.798 14-26.718 14-18.52-.04-33.559-14.72-33.559-32.76C22.4 36.48 37.48 21.8 55.958 21.68c.84 0 1.92.12 3.28.4-5.4-2.32-11.2-4-16.72-4C8.6 18.08 2 40.2 1.52 41.76A52.8 52.8 0 0 0 0 54.397c0 29.999 25.119 54.398 55.958 54.398Z"
              />
              <path
                fill="#29ABE2"
                d="M187.793 90.197c-17.36-.44-35.399-14.12-39.079-17.52-9.519-8.8-31.479-32.599-33.198-34.479C99.436 20.16 77.637 0 55.958 0h-.08C29.558.12 7.44 17.96 1.52 41.758c.44-1.56 9.12-24.119 40.958-23.319 17.36.44 35.479 14.32 39.199 17.72 9.52 8.8 31.479 32.598 33.199 34.478 16.079 18 37.878 38.159 59.557 38.159h.08c26.319-.12 48.478-17.96 54.358-41.759-.48 1.56-9.2 23.92-41.078 23.16Z"
              />
            <g>
          </svg>
          <h1 class="c-logo__type">Internet Identity</h1>
        </div>
        <h1 class="t-title t-title--main">
          Securely connect to dapps on the Internet Computer
        </h1>
      </div>
      <div class="l-sidebar__decoration">${dappsVisual()}</div>
    </div>`,
  });
  const container = document.getElementById("pageContent") as HTMLElement;
  render(template, container);
};

// Register this device as a new device with the anchor
const asNewDevice = async (connection: Connection, userNumber?: bigint) => {
  // Prompt the user for an anchor (only used if we don't know the anchor already)
  const askUserNumber = async () => {
    const result = await promptUserNumber({
      title: "Add a Trusted Device",
      message: "What’s your Internet Identity?",
    });
    if (result === "canceled") {
      // TODO L2-309: do this without reload
      return window.location.reload() as never;
    }

    return result;
  };

  return registerTentativeDevice(
    userNumber ?? (await askUserNumber()),
    connection
  );
};
