import { html, render } from "lit-html";
import { editIcon, icLogo } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import {
  getUserNumber,
  parseUserNumber,
  setUserNumber,
} from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { IIConnection, Connection } from "../../utils/iiConnection";
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
import { unreachable } from "../../utils/utils";

const pageContent = (
  hostName: string,
  userNumber?: bigint,
  derivationOrigin?: string
) => html` <style>
    .anchorText {
      font-size: 1.5rem;
    }

    #userNumberInput {
      margin: 0;
      text-align: center;
      width: 100%;
      box-sizing: border-box;
      padding: 0.5rem 2rem;
    }

    #userNumberInput:focus {
      box-sizing: border-box;
      border-style: double;
      border-width: 2px;
      border-radius: 4px;
      border-image-slice: 1;
      outline: none;
      border-image-source: linear-gradient(
        270.05deg,
        #29abe2 10.78%,
        #522785 22.2%,
        #ed1e79 42.46%,
        #f15a24 59.41%,
        #fbb03b 77.09%
      );
    }

    .childContainer {
      position: relative;
      margin-bottom: 0.5rem;
    }

    #editAnchorButton {
      display: flex;
      flex-direction: column;
      position: absolute;
      top: 0;
      bottom: 0;
      right: 0;
      padding: 0.3rem;
      margin: 0;
      width: 2rem;
      background: transparent;
      border: none;
    }

    #registerButton {
      margin: 0.5rem 0 1rem 0;
    }

    .hostName {
      padding: 1rem 0;
      font-size: 0.8rem;
      font-weight: 400;
    }

    .buttonContainer {
      margin: 0.5rem 0;
    }

    button.primary {
      margin-top: 1rem;
    }

    hr {
      border: none;
      border-top-color: var(--grey-050);
      border-top-style: solid;
      margin: 0.5rem 0;
    }

    .anchor-error-message .error-message {
      margin-bottom: 1rem;
    }

    .hidden {
      display: none;
    }

    .host-name {
      padding: 1rem 0.5rem;
    }

    .container {
      padding: 3.5rem 1rem 2rem;
    }

    @media (min-width: 512px) {
      .container {
        padding: 3.5rem 2.5rem 2rem;
      }
    }
  </style>
  <div class="container">
    ${icLogo}
    <h1>Internet Identity</h1>
    <p>Authenticate to service:</p>
    <div class="host-name highlightBox hostName">${hostName}</div>
    ${derivationOrigin !== undefined && derivationOrigin !== hostName
      ? derivationOriginSection(derivationOrigin)
      : ""}
    <p>Use Identity Anchor:</p>

    <div class="childContainer">
      <input
        class="anchorText"
        type="text"
        id="userNumberInput"
        placeholder="Enter anchor"
        value="${userNumber !== undefined ? userNumber : ""}"
      />
      <button id="editAnchorButton">${editIcon}</button>
    </div>
    <div
      id="invalidAnchorMessage"
      class="anchor-error-message error-message-hidden"
    >
      The Identity Anchor is not valid. Please try again.
    </div>

    <button type="button" id="authorizeButton" class="primary">
      Start Session
    </button>
    <div id="registerSection">
      <button type="button" id="registerButton">
        Create New Identity Anchor
      </button>
    </div>
    <div class="textLink">
      <hr />
      <div class="buttonContainer">
        <a id="recoverButton" class="linkStyle">Lost access?</a>
      </div>
      <div class="buttonContainer">
        <a type="button" class="linkStyle" id="manageButton">
          Manage your Identity Anchor
        </a>
      </div>
      <hr />
    </div>
    ${navbar}
  </div>
  ${footer}`;

const derivationOriginSection = (derivationOrigin: string) => html` <p>
    This service is an alias of:
  </p>
  <div class="host-name highlightBox hostName">${derivationOrigin}</div>`;

export interface AuthSuccess {
  userNumber: bigint;
  connection: IIConnection;
  sendDelegationMessage: () => void;
}

/**
 * Shows the authorize application view and returns information about the authenticated user and a callback, which sends
 * the delegation to the application window. After having received the delegation the application will close the
 * Internet Identity window.
 */
// TODO: no default export
export default async (conn: Connection): Promise<AuthSuccess> => {
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
        init(conn, authContext, userNumber).then(resolve);
      });
    default:
      unreachable(validationResult);
      break;
  }
};

const init = (
  conn: Connection,
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  displayPage(
    authContext.requestOrigin,
    userNumber,
    authContext.authRequest.derivationOrigin
  );
  initManagementBtn();
  initRecovery(conn);

  const authorizeButton = document.getElementById(
    "authorizeButton"
  ) as HTMLButtonElement;
  const userNumberInput = document.getElementById(
    "userNumberInput"
  ) as HTMLInputElement;
  const editAnchorButton = document.getElementById(
    "editAnchorButton"
  ) as HTMLButtonElement;
  const registerSection = document.getElementById(
    "registerSection"
  ) as HTMLInputElement;

  userNumberInput.onkeypress = (e) => {
    if (e.key === "Enter") {
      // authenticate if user hits enter
      e.preventDefault();
      authorizeButton.click();
    }
  };

  // only use non-edit mode if the anchor is set and was previously used successfully (i.e. is in local storage)
  if (userNumber !== undefined && userNumber === getUserNumber()) {
    userNumberInput.classList.add("highlightBox");
    registerSection.classList.add("hidden");
    userNumberInput.disabled = true;
    authorizeButton.focus();
  } else {
    editAnchorButton.classList.add("hidden");
    userNumberInput.select();
  }
  editAnchorButton.onclick = () => {
    editAnchorButton.classList.add("hidden");
    registerSection.classList.remove("hidden");
    userNumberInput.classList.remove("highlightBox");
    userNumberInput.disabled = false;
    userNumberInput.select();
  };

  return new Promise((resolve) => {
    // Resolve either on successful authentication or after registration
    initRegistration(conn, authContext, userNumber).then(resolve);
    authorizeButton.onclick = () => {
      authenticateUser(conn, authContext).then((authSuccess) => {
        if (authSuccess !== null) {
          resolve(authSuccess);
        }
      });
    };
  });
};

function initManagementBtn() {
  const manageButton = document.getElementById(
    "manageButton"
  ) as HTMLAnchorElement;
  manageButton.onclick = () => {
    window.location.hash = "";
    window.location.reload();
  };
}

const initRegistration = async (
  conn: Connection,
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  return new Promise((resolve) => {
    registerButton.onclick = () => {
      registerIfAllowed(conn)
        .then((result) => {
          if (result === null) {
            // user canceled registration
            return init(conn, authContext, userNumber);
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
          }).then(() => init(conn, authContext, userNumber));
        })
        .then(resolve);
    };
  });
};

const authenticateUser = async (
  conn: Connection,
  authContext: AuthContext
): Promise<AuthSuccess | null> => {
  const userNumber = readUserNumber();
  try {
    if (userNumber === undefined) {
      toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
      return null;
    }
    const result = await withLoader(() => conn.login(userNumber));
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
  return init(conn, authContext, userNumber);
};

const displayPage = (
  origin: string,
  userNumber?: bigint,
  derivationOrigin?: string
) => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(origin, userNumber, derivationOrigin), container);
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

const initRecovery = (conn: Connection) => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLAnchorElement;
  recoverButton.onclick = () => useRecovery(conn, readUserNumber());
};

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
