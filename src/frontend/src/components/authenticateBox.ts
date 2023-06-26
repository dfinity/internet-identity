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

import copyJson from "./authenticateBox.json";

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
        recover: async (_userNumber) => {
          resolve({
            ...(await useRecovery(connection)),
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
const page = (slot: TemplateResult) => {
  const template = mainWindow({
    slot: html` <!-- The title is hidden but used for accessibility -->
      <h1 data-page="authenticate" class="is-hidden">Internet Identity</h1>
      ${slot}`,
  });
  const container = document.getElementById("pageContent") as HTMLElement;
  container.classList.add("l-wrap--spacious");
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
