import { html, render } from "lit-html";
import { editIcon } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import {
  getUserNumber,
  parseUserNumber,
  setUserNumber,
} from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { IIConnection } from "../../utils/iiConnection";
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

const pageContent = (hostName: string, userNumber?: bigint) => html` <style>
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

    .modeContainer {
      min-height: 7rem;
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
      margin: 3rem 0;
    }

    .hostName {
      font-size: 1rem;
      font-weight: 400;
    }

    .sectionTitle {
      margin: 1rem 0 0.5rem 0;
      font-size: 1rem;
      font-weight: normal;
      color: black;
    }

    .buttonContainer {
      margin: 0.5rem 0;
    }

    hr {
      border: none;
      border-top-color: var(--grey-050);
      border-top-style: solid;
      margin: 1rem 0;
    }
  </style>
  <div class="container">
    <h1>Authorize Authentication</h1>
    <h2 class="sectionTitle">Application URL</h2>
    <div class="highlightBox hostName">${hostName}</div>
    <h2 class="sectionTitle">Identity Anchor</h2>
    <div class="modeContainer">
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
      <div id="invalidAnchorMessage" class="error-message-hidden">
        The Identity Anchor is not valid. Please try again.
      </div>
    </div>
    <button type="button" id="authorizeButton" class="primary">
      Authorize
    </button>
    <div id="registerSection">
      <button type="button" id="registerButton">
        Create New Identity Anchor
      </button>
    </div>
    <div class="textLink">
      <hr />
      <div class="buttonContainer">
        <button id="recoverButton" class="linkStyle">Lost access?</button>
      </div>
      <div class="buttonContainer">
        <button type="button" class="linkStyle" id="manageButton">
          Manage your Identity Anchor
        </button>
      </div>
      <hr />
    </div>
    ${navbar}
  </div>
  ${footer}`;

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
export default async (): Promise<AuthSuccess> => {
  return new Promise((resolve) => {
    withLoader(async () => {
      const authContext = await waitForAuthRequest();
      if (authContext === null) {
        // The user has manually navigated to "/#authorize".
        window.location.hash = "";
        window.location.reload();
        return;
      }
      const userNumber = getUserNumber();
      init(authContext, userNumber).then(resolve);
    });
  });
};

const init = (
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  displayPage(authContext.requestOrigin, userNumber);
  initManagementBtn();
  initRecovery();

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

  if (userNumber === undefined) {
    editAnchorButton.classList.add("hidden");
    userNumberInput.select();
  } else {
    userNumberInput.classList.add("highlightBox");
    registerSection.classList.add("hidden");
    userNumberInput.disabled = true;
    authorizeButton.focus();
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
    initRegistration(authContext, userNumber).then(resolve);
    authorizeButton.onclick = () => {
      authenticateUser(authContext).then((authSuccess) => {
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
  ) as HTMLButtonElement;
  manageButton.onclick = () => {
    window.location.hash = "";
    window.location.reload();
  };
}

const initRegistration = async (
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  return new Promise((resolve) => {
    registerButton.onclick = () => {
      registerIfAllowed()
        .then((result) => {
          if (result === null) {
            // user canceled registration
            return init(authContext, userNumber);
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
          }).then(() => init(authContext, userNumber));
        })
        .then(resolve);
    };
  });
};

const authenticateUser = async (
  authContext: AuthContext
): Promise<AuthSuccess | null> => {
  const userNumber = readUserNumber();
  try {
    if (userNumber === undefined) {
      toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
      return null;
    }
    const result = await withLoader(() => IIConnection.login(userNumber));
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
  return init(authContext, userNumber);
};

const displayPage = (origin: string, userNumber?: bigint) => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(origin, userNumber), container);
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

const initRecovery = () => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLButtonElement;
  recoverButton.onclick = () => {
    const userNumber = readUserNumber();
    if (userNumber !== undefined) {
      return useRecovery(userNumber);
    }
    toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
  };
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
