import { html, render, TemplateResult } from "lit-html";
import { icLogo, attentionIcon } from "../../components/icons";
import { footer } from "../../components/footer";
import {
  getUserNumber,
  parseUserNumber,
  setUserNumber,
} from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { mkAnchorInput } from "../../components/anchorInput";
import { AuthenticatedConnection, Connection } from "../../utils/iiConnection";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import {
  apiResultToLoginFlowResult,
  LoginFlowSuccess,
} from "../login/flowResult";
import { displayError } from "../../components/displayError";
import { useRecovery } from "../recovery/useRecovery";
import waitForAuthRequest, { AuthContext } from "./postMessageInterface";
import { toggleErrorMessage } from "../../utils/errorHelper";
import { fetchDelegation } from "./fetchDelegation";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";
import {
  validateDerivationOrigin,
  ValidationResult,
} from "./validateDerivationOrigin";
import { unreachable, withRef } from "../../utils/utils";

type PageElements = {
  authorizeButton: Ref<HTMLButtonElement>;
  userNumberInput: Ref<HTMLInputElement>;
  registerButton: Ref<HTMLLinkElement>;
};

const pageContent = (
  connection: Connection,
  hostName: string,
  userNumber?: bigint,
  derivationOrigin?: string
): PageElements & { template: TemplateResult } => {
  const onManageClick = () => {
    window.location.hash = "";
    window.location.reload();
  };

  const onRecoverClick = () => useRecovery(connection, readUserNumber());

  const authorizeButton: Ref<HTMLButtonElement> = createRef();
  const registerButton: Ref<HTMLLinkElement> = createRef();
  const anchorInput = mkAnchorInput("userNumberInput", userNumber, (e) => {
    if (e.key === "Enter") {
      // authenticate if user hits enter
      e.preventDefault();
      withRef(authorizeButton, (authorizeButton) => authorizeButton.click());
    }
  });

  const template = html` <div class="l-container c-card c-card--highlight">
      <!-- The title is hidden but used for accessibility -->
      <h1 class="is-hidden">Internet Identity</h1>
      <div class="c-logo">${icLogo}</div>
      <div class="l-stack">
        <p class="t-lead" style="text-align: center;">
          Connect to<br />
          <span class="t-strong">${hostName}</span>
          ${derivationOrigin !== undefined && derivationOrigin !== hostName
            ? html` <span class="c-tooltip t-link__icon"
                >${attentionIcon}<i
                  class="c-tooltip__message c-card c-card--narrow"
                  >An alternative origin of ${derivationOrigin}</i
                ></span
              >`
            : ""}
          <br />
        </p>
      </div>

      ${anchorInput.template}

      <button ${ref(authorizeButton)} id="authorizeButton" class="c-button">
        Authorize
      </button>
      <div class="l-stack">
        <ul class="c-list--flex">
          <li>
            <a ${ref(registerButton)} id="registerButton" class="t-link"
              >Create Anchor</a
            >
          </li>
          <li>
            <a @click="${onRecoverClick}" id="recoverButton" class="t-link"
              >Lost Access?</a
            >
          </li>
          <li>
            <a @click="${onManageClick}" class="t-link" id="manageButton"
              >Manage Anchor</a
            >
          </li>
          <li>
            <a
              class="t-link"
              href="/faq"
              target="_blank"
              rel="noopener noreferrer"
              >FAQ</a
            >
          </li>
        </ul>
      </div>
    </div>
    ${footer}`;

  return {
    template,
    authorizeButton,
    userNumberInput: anchorInput.userNumberInput,
    registerButton,
  };
};

export interface AuthSuccess {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  sendDelegationMessage: () => void;
}

/**
 * Shows the authorize application view and returns information about the authenticated user and a callback, which sends
 * the delegation to the application window. After having received the delegation the application will close the
 * Internet Identity window.
 */
export const authorizeAuthentication = async (
  connection: Connection
): Promise<AuthSuccess> => {
  const [authContext, validationResult]: [AuthContext, ValidationResult] =
    await withLoader(async () => {
      const authContext = await waitForAuthRequest();

      if (authContext === null) {
        // The user has manually navigated to "/#authorize".
        window.location.hash = "";
        window.location.reload();
        return new Promise((_resolve) => {
          // never resolve, window is being reloaded
        });
      }

      const validationResult = await validateDerivationOrigin(
        authContext.requestOrigin,
        authContext.authRequest.derivationOrigin
      );
      return [authContext, validationResult];
    });

  const userNumber = getUserNumber();
  switch (validationResult.result) {
    case "invalid":
      await displayError({
        title: "Invalid Derivation Origin",
        message: `"${authContext.authRequest.derivationOrigin}" is not a valid derivation origin for "${authContext.requestOrigin}"`,
        detail: validationResult.message,
        primaryButton: "Close",
      });

      // notify the client application
      // do this after showing the error because the client application might close the window immediately after receiving the message and might not show the user what's going on
      authContext.postMessageCallback({
        kind: "authorize-client-failure",
        text: `Invalid derivation origin: ${validationResult.message}`,
      });

      // we cannot recover from this, retrying or reloading won't help
      // close the window as it returns the user to the offending application that opened II for authentication
      window.close();
      return new Promise((_resolve) => {
        // never resolve, do not call init
      });
    case "valid":
      return new Promise((resolve) => {
        init(connection, authContext, userNumber).then(resolve);
      });
    default:
      unreachable(validationResult);
      break;
  }
};

const init = (
  connection: Connection,
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  const { authorizeButton, userNumberInput, registerButton } = displayPage(
    connection,
    authContext.requestOrigin,
    userNumber,
    authContext.authRequest.derivationOrigin
  );

  // only focus on the button if the anchor is set and was previously used successfully (i.e. is in local storage)
  if (userNumber !== undefined && userNumber === getUserNumber()) {
    withRef(authorizeButton, (authorizeButton) => authorizeButton.focus());
  } else {
    withRef(userNumberInput, (userNumberInput) => userNumberInput.select());
  }

  return new Promise((resolve) => {
    // Resolve either on successful authentication or after registration
    withRef(registerButton, (registerButton) =>
      initRegistration(
        connection,
        authContext,
        registerButton,
        userNumber
      ).then(resolve)
    );
    withRef(authorizeButton, (authorizeButton) => {
      authorizeButton.onclick = () => {
        authenticateUser(connection, authContext).then((authSuccess) => {
          if (authSuccess !== null) {
            resolve(authSuccess);
          }
        });
      };
    });
  });
};

const initRegistration = async (
  connection: Connection,
  authContext: AuthContext,
  registerButton: HTMLLinkElement,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  return new Promise((resolve) => {
    registerButton.onclick = () => {
      registerIfAllowed(connection)
        .then((result) => {
          if (result === null) {
            // user canceled registration
            return init(connection, authContext, userNumber);
          }
          if (result.tag === "ok") {
            return handleAuthSuccess(result, authContext);
          }
          // something went wrong, display error and try again
          return displayError({
            title: result.title,
            message: result.message,
            detail: result.detail !== "" ? result.detail : undefined,
            primaryButton: "Try again",
          }).then(() => init(connection, authContext, userNumber));
        })
        .then(resolve);
    };
  });
};

const authenticateUser = async (
  connection: Connection,
  authContext: AuthContext
): Promise<AuthSuccess | null> => {
  const userNumber = readUserNumber();
  try {
    if (userNumber === undefined) {
      toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
      return null;
    }
    const result = await withLoader(() => connection.login(userNumber));
    const loginResult = apiResultToLoginFlowResult(result);
    if (loginResult.tag === "ok") {
      return await withLoader(() =>
        handleAuthSuccess(loginResult, authContext)
      );
    }
    await displayError({
      title: loginResult.title,
      message: loginResult.message,
      detail: loginResult.detail !== "" ? loginResult.detail : undefined,
      primaryButton: "Try again",
    });
  } catch (error) {
    await displayError({
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: error instanceof Error ? error.message : JSON.stringify(error),
      primaryButton: "Try again",
    });
  }
  return init(connection, authContext, userNumber);
};

export const displayPage = (
  connection: Connection,
  origin: string,
  userNumber?: bigint,
  derivationOrigin?: string
): PageElements => {
  const container = document.getElementById("pageContent") as HTMLElement;
  const ret = pageContent(connection, origin, userNumber, derivationOrigin);
  render(ret.template, container);

  return ret;
};

async function handleAuthSuccess(
  loginResult: LoginFlowSuccess,
  authContext: AuthContext
) {
  // successful login, store user number for next time
  setUserNumber(loginResult.userNumber);
  const [userKey, parsed_signed_delegation] = await fetchDelegation(
    loginResult,
    authContext
  );
  return {
    userNumber: loginResult.userNumber,
    connection: loginResult.connection,
    sendDelegationMessage: () =>
      authContext.postMessageCallback({
        kind: "authorize-client-success",
        delegations: [parsed_signed_delegation],
        userPublicKey: Uint8Array.from(userKey),
      }),
  };
}

/**
 * Read and parse the user number from the input field.
 */
const readUserNumber = () => {
  const parsedUserNumber = parseUserNumber(
    (document.getElementById("userNumberInput") as HTMLInputElement).value
  );
  // get rid of null, we use undefined for 'not set'
  return parsedUserNumber === null ? undefined : parsedUserNumber;
};
