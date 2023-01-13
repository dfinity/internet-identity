import { html, render, TemplateResult } from "lit-html";
import { NonEmptyArray } from "../utils/utils";
import { icLogo } from "./icons";
import { footer } from "./footer";
import { withLoader } from "./loader";
import { addRemoteDevice } from "../flows/addDevice/welcomeView";
import { mkAnchorPicker } from "./anchorPicker";
import { mkAnchorInput } from "./anchorInput";
import { getAnchors, setAnchorUsed } from "../utils/userNumber";
import { unreachable, isNonEmptyArray } from "../utils/utils";
import { Connection } from "../utils/iiConnection";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
  LoginFlowSuccess,
  LoginFlowError,
  LoginData,
} from "../utils/flowResult";
import { displayError } from "./displayError";
import { useRecovery } from "../flows/recovery/useRecovery";
import { registerIfAllowed } from "../utils/registerAllowedCheck";

/** Template used for rendering specific authentication screens. See `authnPages` below
 * for meaning of "firstTime", "useExisting" and "pick". */
export type AuthnTemplates = {
  firstTime: {
    slot: TemplateResult;
    useExistingText: string /** text shown on the button leading to "useExisting" */;
    createAnchorText: string /** text shown on the button leading to anchor creation */;
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
  templates: AuthnTemplates
): Promise<LoginData> => {
  const promptAuth = () =>
    new Promise<LoginFlowResult>((resolve) => {
      const pages = authnPages({
        ...templates,
        addDevice: () => addRemoteDevice(connection),
        onSubmit: (userNumber) => {
          resolve(authenticate(connection, userNumber));
        },
        register: () => {
          resolve(registerIfAllowed(connection));
        },
        recover: () => useRecovery(connection),
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
    const result = await promptAuth();
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
  }
};

/** The authentication pages, namely "firstTime" (for new users), "useExisting" (for users who
 * don't have saved anchors or who wish to use non-saved anchors) and "pick" (for users
 * picking a saved anchor) */
export const authnPages = (
  props: {
    register: () => void;
    onSubmit: (anchor: bigint) => void;
    addDevice: () => void;
    recover: () => void;
  } & AuthnTemplates
) => ({
  firstTime: (firstTimeProps: { useExisting: () => void }) =>
    page(html`${props.firstTime.slot}
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
      <p class="t-paragraph t-centered l-stack">
        <a
          class="t-link"
          href="https://medium.com/dfinity/internet-identity-the-end-of-usernames-and-passwords-ff45e4861bf7"
          target="_blank"
          rel="noopener noreferrer"
          >Learn More</a
        >
      </p> `),
  useExisting: () => {
    const anchorInput = mkAnchorInput({ onSubmit: props.onSubmit });
    page(html` ${props.useExisting.slot} ${anchorInput.template}
      <ul class="c-list--flex">
        <li>
          <a
            @click="${() => props.addDevice()}"
            id="addNewDeviceButton"
            class="t-link"
            >Add a new device?</a
          >
        </li>
        <li>
          <a @click="${() => props.recover()}" id="recoverButton" class="t-link"
            >Lost Access?</a
          >
        </li>
      </ul>
      <div class="c-button-group">
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
      </div>`);
  },
  pick: (pickProps: {
    anchors: NonEmptyArray<bigint>;
    moreOptions: () => void;
  }) =>
    page(
      html`
        ${props.pick.slot}
        ${mkAnchorPicker({
          savedAnchors: pickProps.anchors,
          pick: props.onSubmit,
          moreOptions: pickProps.moreOptions,
        }).template}
      `
    ),
});

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
  const template = html`
    <div class="l-container">
      <div class="c-logo">${icLogo}</div>
      <div class="c-card c-card--background">
        <div class="c-card c-card--highlight">
          <!-- The title is hidden but used for accessibility -->
          <h1 data-page="authenticate" class="is-hidden">Internet Identity</h1>
          
          ${slot}
        </div>
        ${footer}
      </div>
    </div>
  `;

  const container = document.getElementById("pageContent") as HTMLElement;
  render(template, container);
};
