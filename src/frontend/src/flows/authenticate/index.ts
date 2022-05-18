import { html, render } from "lit-html";
import { gearIcon, undoIcon, userSwitchIcon } from "../../components/icons";
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
import { recoveryWizard } from "../recovery/recoveryWizard";
import { useRecovery } from "../recovery/useRecovery";
import {
  initRegistration,
  registrationSection,
} from "../../components/registrationLink";
import { AuthContext, READY_MESSAGE } from "./postMessageInterface";
import {
  PublicKey,
  SignedDelegation,
} from "../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../utils/utils";
import { toggleErrorMessage } from "../../utils/errorHelper";
import { questions } from "../faq";

const pageContent = (
  hostName: string,
  maxTimeToLive: bigint | undefined,
  userNumber?: bigint
) => html` <style>
    .spacer-small {
      height: 1rem;
    }

    #gearIcon {
      position: relative;
      top: 4px;
    }

    .spacer {
      height: 3rem;
    }

    .mediumText {
      font-size: 1.2rem;
    }

    #userNumberInput {
      margin: 0;
      text-align: center;
      width: 100%;
      box-sizing: border-box;
      padding: 0.55rem 3rem;
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

    .switchButton {
      display: flex;
      flex-direction: column;
      position: absolute;
      top: 0;
      bottom: 0;
      right: 0;
      padding: 0.7rem;
      margin: 0;
      width: fit-content;
      background: transparent;
      border: none;
    }

    .smallText {
      font-size: 0.875rem;
      font-weight: 400;
    }

    .bold {
      font-weight: 600;
    }

    .greyText {
      color: var(--grey-500);
    }

    .anchorTitle {
      margin: 0;
      color: black;
    }
  </style>
  <div class="container">
    <h1>Authorize Authentication for</h1>
    <div class="highlightBox smallText">${hostName}</div>
    <div class="spacer-small"></div>
    <p class="anchorTitle">Identity Anchor</p>
    <div>
      <div id="newUserNumber" class="modeContainer">
        <div class="childContainer">
          <input
            class="mediumText"
            type="text"
            id="userNumberInput"
            placeholder="Enter Identity Anchor"
          />
          <button
            id="existingAnchorButton"
            class="${userNumber === undefined ? "hidden" : ""} switchButton"
          >
            ${undoIcon}
          </button>
        </div>
        <div id="invalidAnchorMessage" class="error-message hidden smallText">
          The Identity Anchor is not valid. Please try again.
        </div>
        ${registrationSection}
      </div>
      <div id="existingUserNumber" class="modeContainer">
        <div class="childContainer">
          <div id="identityAnchor" class="highlightBox mediumText">
            ${userNumber}
          </div>
          <button id="editAnchorButton" class="switchButton">
            ${userSwitchIcon}
          </button>
        </div>
      </div>
    </div>
    <button type="button" id="login" class="primary">Authenticate</button>
    <div class="smallText greyText">
      Max session validity: ${formatTimeToLive(maxTimeToLive)}
    </div>
    <div class="smallText greyText">
      Different applications will not be able to track your activities among
      each other.
      <a
        class="bold greyText"
        href="/faq#${questions.shareIIAnchor.anchor}"
        target="_blank"
        >Learn More.</a
      >
    </div>
    <div class="spacer"></div>
    <div class="textLink">
      Lost access
      <button id="recoverButton" class="linkStyle">and want to recover?</button>
    </div>
    <div id="logoutBox">
      <hr />
      <button type="button" class="linkStyle" id="manageButton">
        ${gearIcon} Manage your Identity Anchor
      </button>
    </div>
    ${navbar}
  </div>
  ${footer}`;

export const authenticate = async (): Promise<void> => {
  await withLoader(async () => {
    const authContext = await setup();
    return init(authContext);
  });
};

/**
 * Setup an event listener to listen to authorize requests from the client.
 */
export default async function setup(): Promise<AuthContext> {
  const result = new Promise<AuthContext>((resolve, reject) => {
    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", async (event) => {
      const message = event.data;
      if (message.kind === "authorize-client") {
        console.log("Handling authorize-client request.");
        resolve(
          new AuthContext(message, event.origin, (responseMessage) =>
            (event.source as Window).postMessage(responseMessage, event.origin)
          )
        );
      } else {
        console.error(
          `Message of unknown kind received: ${JSON.stringify(message)}`
        );
        reject();
      }
    });
  });

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  if (window.opener !== null) {
    window.opener.postMessage(READY_MESSAGE, "*");
  } else {
    redirectToWelcomeScreen();
  }
  return result;
}

const init = (authContext: AuthContext) => {
  const userNumber = getUserNumber();
  displayPage(
    authContext.requestOrigin,
    authContext.authRequest.maxTimeToLive,
    userNumber
  );
  initRecovery();
  initRegistration().then((result) =>
    result === null
      ? init(authContext)
      : handleAuthenticationResult(result, authContext)
  );
  const editAnchorButton = document.getElementById(
    "editAnchorButton"
  ) as HTMLButtonElement;
  editAnchorButton.onclick = () => setMode("newUserNumber");
  const existingAnchorButton = document.getElementById(
    "existingAnchorButton"
  ) as HTMLButtonElement;
  existingAnchorButton.onclick = () => setMode("existingUserNumber");

  const loginButton = document.querySelector("#login") as HTMLButtonElement;
  loginButton.onclick = async () => {
    try {
      const userNumber = readUserNumber();
      if (userNumber === undefined) {
        toggleErrorMessage("userNumberInput", "invalidAnchorMessage", true);
        return;
      }
      const result = await withLoader(() => IIConnection.login(userNumber));
      const loginResult = apiResultToLoginFlowResult(result);
      await handleAuthenticationResult(loginResult, authContext);
    } catch (error) {
      await displayError({
        title: "Authentication Failed",
        message: "An error occurred during authentication.",
        detail: error instanceof Error ? error.message : JSON.stringify(error),
        primaryButton: "Try again",
      });
      init(authContext);
    }
  };
  document.onkeypress = (e) => {
    if (e.key === "Enter") {
      // authenticate if user hits enter
      e.preventDefault();
      loginButton.click();
    }
  };

  const manageButton = document.getElementById(
    "manageButton"
  ) as HTMLButtonElement;
  manageButton.onclick = () => redirectToWelcomeScreen();
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

const handleAuthenticationResult = async (
  loginResult: LoginFlowResult,
  authContext: AuthContext
) => {
  if (loginResult.tag === "ok") {
    setUserNumber(loginResult.userNumber); // successful login, store user number for next time
    const { userKey, parsed_signed_delegation } = await fetchDelegation(
      loginResult,
      authContext
    );

    // show the recovery wizard before sending the window post message, otherwise the II window will be closed
    await recoveryWizard(loginResult.userNumber, loginResult.connection);

    // send the
    authContext.postMessageCallback({
      kind: "authorize-client-success",
      delegations: [parsed_signed_delegation],
      userPublicKey: Uint8Array.from(userKey),
    });
  } else {
    await displayError({
      title: loginResult.title,
      message: loginResult.message,
      detail: loginResult.detail !== "" ? loginResult.detail : undefined,
      primaryButton: "Try again",
    });
    init(authContext);
  }
};

const fetchDelegation = async (
  loginResult: {
    userNumber: bigint;
    connection: IIConnection;
  },
  authContext: AuthContext
) => {
  const [userKey, parsed_signed_delegation] = await withLoader(async () => {
    const sessionKey = Array.from(authContext.authRequest.sessionPublicKey);
    const [userKey, timestamp] = await loginResult.connection.prepareDelegation(
      loginResult.userNumber,
      authContext.requestOrigin,
      sessionKey,
      authContext.authRequest.maxTimeToLive
    );

    const signed_delegation = await retryGetDelegation(
      loginResult.connection,
      loginResult.userNumber,
      authContext.requestOrigin,
      sessionKey,
      timestamp
    );

    // Parse the candid SignedDelegation into a format that `DelegationChain` understands.
    return [
      userKey,
      {
        delegation: {
          pubkey: Uint8Array.from(signed_delegation.delegation.pubkey),
          expiration: BigInt(signed_delegation.delegation.expiration),
          targets: undefined,
        },
        signature: Uint8Array.from(signed_delegation.signature),
      },
    ];
  });
  return { userKey, parsed_signed_delegation };
};

const retryGetDelegation = async (
  connection: IIConnection,
  userNumber: bigint,
  hostname: string,
  sessionKey: PublicKey,
  timestamp: bigint,
  maxRetries = 5
): Promise<SignedDelegation> => {
  for (let i = 0; i < maxRetries; i++) {
    // Linear backoff
    await new Promise((resolve) => {
      setInterval(resolve, 1000 * i);
    });
    const res = await connection.getDelegation(
      userNumber,
      hostname,
      sessionKey,
      timestamp
    );
    if (hasOwnProperty(res, "signed_delegation")) {
      return res.signed_delegation;
    }
  }
  throw new Error(
    `Failed to retrieve a delegation after ${maxRetries} retries.`
  );
};

const initRecovery = () => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLButtonElement;
  recoverButton.onclick = () => useRecovery(getUserNumber());
};

const setMode = (mode: "existingUserNumber" | "newUserNumber") => {
  const existingUserNumber = document.getElementById(
    "existingUserNumber"
  ) as HTMLElement;
  const newUserNumber = document.getElementById("newUserNumber") as HTMLElement;
  existingUserNumber.classList.toggle("hidden", mode !== "existingUserNumber");
  newUserNumber.classList.toggle("hidden", mode !== "newUserNumber");

  if (mode === "newUserNumber") {
    toggleErrorMessage("userNumberInput", "invalidAnchorMessage", false);
    const userNumberInput = document.getElementById(
      "userNumberInput"
    ) as HTMLInputElement;
    userNumberInput.focus();
  }
};

const redirectToWelcomeScreen = () => {
  // If there's no `window.opener` a user has manually navigated to "/#authorize". Let's
  // redirect them to the non-hash version.
  window.location.hash = "";
  window.location.reload();
};

const formatTimeToLive = (maxTimeToLive: bigint | undefined) => {
  const seconds =
    maxTimeToLive !== undefined
      ? Math.floor(Number(maxTimeToLive / BigInt(1_000_000_000)))
      : 30 * 60; // 30 minutes default
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);
  if (days > 0) {
    return formatWithSecondaryTimeUnit(days, "day", hours % 24, "hour");
  }
  if (hours > 0) {
    return formatWithSecondaryTimeUnit(hours, "hour", minutes % 60, "minute");
  }
  if (minutes > 0) {
    return formatWithSecondaryTimeUnit(
      minutes,
      "minute",
      seconds % 60,
      "second"
    );
  }
  return formatTimeUnit(seconds, "second");
};

const formatWithSecondaryTimeUnit = (
  primaryAmount: number,
  primaryUnit: string,
  secondaryAmount: number,
  secondaryUnit: string
) => {
  let result = formatTimeUnit(primaryAmount, primaryUnit);
  if (secondaryAmount > 0) {
    result = result + `, ${formatTimeUnit(secondaryAmount, secondaryUnit)}`;
  }
  return result;
};

const formatTimeUnit = (amount: number, unit: string) => {
  const roundedAmount = Math.floor(amount);
  return `${roundedAmount} ${unit}${roundedAmount > 1 ? "s" : ""}`;
};

const readUserNumber = () => {
  const userNumberInputSection = document.getElementById(
    "newUserNumber"
  ) as HTMLElement;
  if (!userNumberInputSection.classList.contains("hidden")) {
    const parsedUserNumber = parseUserNumber(
      (document.getElementById("userNumberInput") as HTMLInputElement).value
    );
    return parsedUserNumber === null ? undefined : parsedUserNumber;
  }
  return getUserNumber();
};
