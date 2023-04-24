import { html, render, TemplateResult } from "lit-html";
import { PORTAL_II_URL } from "../config";
import { registerTentativeDevice } from "../flows/addDevice/welcomeView/registerTentativeDevice";
import { useRecovery } from "../flows/recovery/useRecovery";
import {
  apiResultToLoginFlowResult,
  LoginData,
  LoginFlowError,
  LoginFlowResult,
  LoginFlowSuccess,
} from "../utils/flowResult";
import { Connection } from "../utils/iiConnection";
import { TemplateElement, withRef } from "../utils/lit-html";
import { registerIfAllowed } from "../utils/registerAllowedCheck";
import {
  getAnchors,
  parseUserNumber,
  setAnchorUsed,
} from "../utils/userNumber";
import { isNonEmptyArray, NonEmptyArray, unreachable } from "../utils/utils";
import { mkAnchorInput } from "./anchorInput";
import { mkAnchorPicker } from "./anchorPicker";
import { displayError } from "./displayError";
import { withLoader } from "./loader";
import { mainWindow } from "./mainWindow";
import { promptUserNumber } from "./promptUserNumber";

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
  templates: AuthnTemplates
): Promise<LoginData> => {
  const promptAuth = () =>
    new Promise<LoginFlowResult>((resolve) => {
      const pages = authnPages({
        ...templates,
        addDevice: (userNumber) => asNewDevice(connection, userNumber),
        onSubmit: (userNumber) => {
          resolve(authenticate(connection, userNumber));
        },
        register: () => {
          resolve(registerIfAllowed(connection));
        },
        recover: (userNumber) => resolve(useRecovery(connection, userNumber)),
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

/** The templates for the authentication pages */
export const authnTemplates = (
  props: {
    register: () => void;
    onSubmit: (anchor: bigint) => void;
    addDevice: (anchor?: bigint) => void;
    recover: (anchor?: bigint) => void;
  } & AuthnTemplates
) => ({
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
      <p class="t-paragraph t-centered l-stack">
        <a
          class="t-link"
          href=${PORTAL_II_URL}
          target="_blank"
          rel="noopener noreferrer"
          >Learn More</a
        >
      </p>`;
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
            >Add a new device?</a
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
});

/** The authentication pages, namely "firstTime" (for new users), "useExisting" (for users who
 * don't have saved anchors or who wish to use non-saved anchors) and "pick" (for users
 * picking a saved anchor) */
export const authnPages = (
  props: {
    register: () => void;
    onSubmit: (anchor: bigint) => void;
    addDevice: (anchor?: bigint) => void;
    recover: (anchor?: bigint) => void;
  } & AuthnTemplates
) => {
  const tpls = authnTemplates(props);
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
  render(template, container);
};

// Register this device as a new device with the anchor
const asNewDevice = async (connection: Connection, userNumber?: bigint) => {
  // Prompt the user for an anchor (only used if we don't know the anchor already)
  const askUserNumber = async () => {
    const result = await promptUserNumber({
      title: "Add a Trusted Device",
      message: "What’s your Identity Anchor?",
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
