import { html, render } from "lit-html";
import { editIcon, undoIcon } from "../../components/icons";
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
  LoginFlowResult,
} from "../login/flowResult";
import { displayError } from "../../components/displayError";
import { useRecovery } from "../recovery/useRecovery";
import { initRegistration } from "../../components/registrationLink";
import waitForAuthRequest, {
  AuthContext,
  delegationMessage,
} from "./postMessageInterface";
import { toggleErrorMessage } from "../../utils/errorHelper";
import { fetchDelegation } from "./fetchDelegation";

const pageContent = (
  hostName: string,
  maxTimeToLive: bigint | undefined,
  userNumber?: bigint
) => html` <style>
    .spacer {
      height: 2rem;
    }

    .spacer-small {
      height: 1rem;
    }

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
      min-height: 6rem;
    }

    .childContainer {
      position: relative;
      margin-bottom: 0.5rem;
    }

    .switchButton {
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

    .smallText {
      font-size: 0.875rem;
      font-weight: 400;
    }

    .sectionTitle {
      margin: 1rem 0 0.5rem 0;
      color: black;
    }

    hr {
      border-color: var(--grey-050);
      margin: 1rem 0;
    }

    .not-displayed {
      display: none;
    }
  </style>
  <div class="container">
    <h1>Authorize private login</h1>
    <p class="sectionTitle">Application URL</p>
    <div class="highlightBox smallText">${hostName}</div>
    <p class="sectionTitle">Identity Anchor</p>
    <div>
      <div id="newUserSection" class="modeContainer">
        <div class="childContainer">
          <input
            class="anchorText"
            type="text"
            id="userNumberInput"
            placeholder="Enter anchor"
          />
          ${userNumber !== undefined
            ? html` <button id="existingAnchorButton" class="switchButton">
                ${undoIcon}
              </button>`
            : ""}
        </div>
        <div id="invalidAnchorMessage" class="error-message-hidden smallText">
          The Identity Anchor is not valid. Please try again.
        </div>
      </div>
      <div id="existingUserSection" class="modeContainer">
        <div class="childContainer">
          <div id="identityAnchor" class="highlightBox anchorText">
            ${userNumber}
          </div>
          <button id="editAnchorButton" class="switchButton">
            ${editIcon}
          </button>
        </div>
      </div>
    </div>
    <button type="button" id="authorizeButton" class="primary">
      Authorize
    </button>
    <div class="spacer"></div>
    <div id="registerSection">
      <div style="text-align: center">or</div>
      <div class="spacer"></div>
      <button type="button" id="registerButton">
        Create New Identity Anchor
      </button>
      <div class="spacer-small"></div>
    </div>
    <div class="textLink">
      <hr />
      <div>
        <button id="recoverButton" class="linkStyle">Lost access?</button>
      </div>
      <div>
        <button type="button" class="linkStyle" id="manageButton">
          Manage your Identity Anchor
        </button>
      </div>
      <hr />
    </div>
    ${navbar}
  </div>
  ${footer}`;

export class AuthSuccess {
  constructor(
    public userNumber: bigint,
    public connection: IIConnection,
    public sendDelegationMessage: () => void
  ) {}
}

/**
 * Shows the authorize application view and returns information about the authenticated user and a callback, which sends
 * the delegation to the application window. After having received the delegation the application will close the
 * Internet Identity window.
 */
export const authenticate = async (): Promise<AuthSuccess> => {
  return new Promise((resolve) => {
    withLoader(async () => {
      const authContext = await waitForAuthRequest();
      if (authContext === null) {
        // The user has manually navigated to "/#authorize".
        redirectToWelcomeScreen();
        return;
      }
      resolve(Promise.resolve(init(authContext)));
    });
  });
};

const init = (authContext: AuthContext): Promise<AuthSuccess> => {
  const userNumber = getUserNumber();
  displayPage(
    authContext.requestOrigin,
    authContext.authRequest.maxTimeToLive,
    userNumber
  );
  const manageButton = document.getElementById(
    "manageButton"
  ) as HTMLButtonElement;
  manageButton.onclick = () => redirectToWelcomeScreen();
  initRecovery();

  const editAnchorButton = document.getElementById(
    "editAnchorButton"
  ) as HTMLButtonElement;
  editAnchorButton.onclick = () => setMode("newUserNumber");

  const existingAnchorButton = document.getElementById("existingAnchorButton");
  if (existingAnchorButton !== null) {
    existingAnchorButton.onclick = () => setMode("existingUserNumber");
  }

  const authenticateButton = document.querySelector(
    "#authorizeButton"
  ) as HTMLButtonElement;
  document.onkeypress = (e) => {
    if (e.key === "Enter") {
      // authenticate if user hits enter
      e.preventDefault();
      authenticateButton.click();
    }
  };

  // Resolve either on successful authentication or after registration
  return new Promise((resolve) => {
    initRegistration()
      .then((result) => {
        if (result === null) {
          // user canceled registration
          return Promise.resolve(init(authContext));
        }
        return Promise.resolve(handleAuthResult(result, authContext));
      })
      .then((authSuccess) => resolve(authSuccess));

    authenticateButton.onclick = () => {
      authenticateUser(authContext).then((authSuccess) => {
        if (authSuccess !== null) {
          resolve(authSuccess);
        }
      });
    };
  });
};

const authenticateUser = async (
  authContext: AuthContext
): Promise<AuthSuccess | null> => {
  try {
    const userNumber = readUserNumber();
    if (userNumber === undefined) {
      toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
      return null;
    }
    const result = await withLoader(() => IIConnection.login(userNumber));
    const loginResult = apiResultToLoginFlowResult(result);
    return await handleAuthResult(loginResult, authContext);
  } catch (error) {
    await displayError({
      title: "Authentication Failed",
      message: "An error occurred during authentication.",
      detail: error instanceof Error ? error.message : JSON.stringify(error),
      primaryButton: "Try again",
    });
    return init(authContext);
  }
};

const displayPage = (
  origin: string,
  maxTimeToLive: bigint | undefined,
  userNumber: bigint | undefined
) => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(origin, maxTimeToLive, userNumber), container);
  setMode(userNumber === undefined ? "newUserNumber" : "existingUserNumber");
};

/**
 * Checks whether login was successful, and if so fetches the delegation from II.
 * @param loginResult Result of authentication
 * @param authContext Information about the authentication request being processed.
 */
const handleAuthResult = async (
  loginResult: LoginFlowResult,
  authContext: AuthContext
): Promise<AuthSuccess> => {
  if (loginResult.tag === "ok") {
    // successful login, store user number for next time
    setUserNumber(loginResult.userNumber);
    const [userKey, parsed_signed_delegation] = await withLoader(() =>
      fetchDelegation(loginResult, authContext)
    );
    return new AuthSuccess(loginResult.userNumber, loginResult.connection, () =>
      authContext.postMessageCallback(
        delegationMessage(parsed_signed_delegation, userKey)
      )
    );
  } else {
    await displayError({
      title: loginResult.title,
      message: loginResult.message,
      detail: loginResult.detail !== "" ? loginResult.detail : undefined,
      primaryButton: "Try again",
    });
    return init(authContext);
  }
};

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
 * Toggles the view between showing a prefilled anchor number and the input field to provide a new one.
 * @param mode Mode to set the view to.
 */
const setMode = (mode: "existingUserNumber" | "newUserNumber") => {
  const existingUserNumber = document.getElementById("existingUserSection");
  const newUserNumber = document.getElementById("newUserSection");
  const registerSection = document.getElementById("registerSection");
  existingUserNumber?.classList.toggle(
    "not-displayed",
    mode !== "existingUserNumber"
  );
  newUserNumber?.classList.toggle("not-displayed", mode !== "newUserNumber");
  registerSection?.classList.toggle("not-displayed", mode !== "newUserNumber");

  if (mode === "newUserNumber") {
    toggleErrorMessage("userNumberInput", "invalidAnchorMessage", false);
    const userNumberInput = document.getElementById(
      "userNumberInput"
    ) as HTMLInputElement;
    userNumberInput.focus();
  }
};

const redirectToWelcomeScreen = () => {
  window.location.hash = "";
  window.location.reload();
};

/**
 * Figure out the current user number to use, depending on the mode the view is in and whether there is a valid value
 * in the input field.
 */
const readUserNumber = () => {
  const newUserSection = document.getElementById(
    "newUserSection"
  ) as HTMLElement;
  if (!newUserSection.classList.contains("not-displayed")) {
    const parsedUserNumber = parseUserNumber(
      (document.getElementById("userNumberInput") as HTMLInputElement).value
    );
    // get rid of null, we use undefined for 'not set'
    return parsedUserNumber === null ? undefined : parsedUserNumber;
  }
  return getUserNumber();
};
