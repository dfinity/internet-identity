import { html, render } from "lit-html";
import { undo, userSwitch } from "../../components/icons";
import { initLogout, logoutSection } from "../../components/logout";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import {
  getUserNumber,
  parseUserNumber,
  setUserNumber,
} from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import setup, { AuthContext, retryGetDelegation } from "../../auth";
import { IIConnection } from "../../utils/iiConnection";
import { apiResultToLoginFlowResult } from "./flowResult";
import { displayError } from "../../components/displayError";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { useRecovery } from "../recovery/useRecovery";
import {
  initRegistration,
  registrationSection,
} from "../../components/registrationLink";

const pageContent = (
  hostName: string,
  maxTimeToLive: bigint | undefined,
  userNumber?: bigint,
) => html` <style>
    .spacer {
      height: 2rem;
    }

    #userNumberInput {
      text-align: center;
      font-size: 1.5rem;
      font-weight: 500;
    }

    .userNumberInput:focus {
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

    .overlap {
    }

    .smallText {
      font-size: 0.875rem;
      font-weight: 400;
    }

    .bold {
      font-weight: 600;
    }
  </style>
  <div class="container">
    <h1>Authorize Authentication for</h1>
    <div class="highlightBox smallText">${hostName}</div>
    <div class="smallText">
      This dapp cannot use Internet Identity to track your activities in other
      dapps.
    </div>
    <div class="spacer"></div>
    <p class="bold">Identity Anchor</p>
    <div>
      <div id="newUserNumber" class="overlap">
        <input id="userNumberInput" placeholder="Enter Identity Anchor" />
        <div id="wrongAnchorMessage" class="error-message-hidden">
          The entered Identity Anchor is invalid. Please try again.
        </div>
        <button
          id="existingAnchorButton"
          class="${userNumber === undefined ? "hidden" : ""}"
        >
          ${undo}
        </button>
        ${registrationSection}
      </div>
      <div id="existingUserNumber" class="overlap">
        <div class="highlightBox">${userNumber}</div>
        <button id="editAnchorButton">${userSwitch}</button>
      </div>
    </div>

    <div class="spacer"></div>
    <button type="button" id="login" class="primary">Authenticate</button>
    <span class="smallText"
      >Max session validity: ${formatTimeToLive(maxTimeToLive)}</span
    >
    <div class="textLink">
      Lost access
      <button id="recoverButton" class="linkStyle">and want to recover?</button>
    </div>
    ${logoutSection("Cancel and clear browser storage")} ${navbar}
  </div>
  ${footer}`;

function displayPage(
  origin: string,
  timeToLive: bigint | undefined,
  userNumber: bigint | undefined,
) {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(origin, timeToLive, userNumber), container);
  setMode(userNumber === undefined ? "newUserNumber" : "existingUserNumber");
}

export const authDapp = async (): Promise<void> => {
  await withLoader(async () => {
    const authContext = await setup();
    return init(authContext);
  });
};

async function authenticate(
  loginResult: {
    userNumber: bigint;
    connection: IIConnection;
  },
  authContext: AuthContext,
) {
  setUserNumber(loginResult.userNumber); // successful login, store for next time
  const [userKey, parsed_signed_delegation] = await withLoader(async () => {
    const sessionKey = Array.from(authContext.authRequest.sessionPublicKey);
    const [userKey, timestamp] = await loginResult.connection.prepareDelegation(
      loginResult.userNumber,
      authContext.requestOrigin,
      sessionKey,
      authContext.authRequest.maxTimeToLive,
    );

    // TODO: Signal failure to retrieve the delegation. Error page, or maybe redirect back with error?
    const signed_delegation = await retryGetDelegation(
      loginResult.connection,
      loginResult.userNumber,
      authContext.requestOrigin,
      sessionKey,
      timestamp,
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
}

async function shubidu(
  loginResult:
    | { tag: "ok"; userNumber: bigint; connection: IIConnection }
    | { tag: "err"; title: string; message: string; detail?: string }
    | null,
  authContext: AuthContext,
) {
  if (loginResult === null) {
    await displayError({
      title: "todo",
      message: "todo",
      primaryButton: "Try again",
    });
    window.location.reload();
    return;
  }
  if (loginResult.tag === "ok") {
    const { userKey, parsed_signed_delegation } = await authenticate(
      loginResult,
      authContext,
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
    await displayError({ ...loginResult, primaryButton: "Try again" });
    init(authContext);
  }
}

const init = (authContext: AuthContext) => {
  const userNumber = getUserNumber();
  displayPage(
    authContext.requestOrigin,
    authContext.authRequest.maxTimeToLive,
    userNumber,
  );
  initLogout();
  initRecovery();
  initRegistration().then((result) => shubidu(result, authContext));
  const editAnchorButton = document.getElementById(
    "editAnchorButton",
  ) as HTMLButtonElement;
  editAnchorButton.onclick = () => setMode("newUserNumber");
  const existingAnchorButton = document.getElementById(
    "existingAnchorButton",
  ) as HTMLButtonElement;
  existingAnchorButton.onclick = () => setMode("existingUserNumber");

  const loginButton = document.querySelector("#login") as HTMLButtonElement;
  loginButton.onclick = async (ev) => {
    ev.preventDefault();
    ev.stopPropagation();
    const result = await withLoader(() => IIConnection.login(readUserNumber()));
    const loginResult = apiResultToLoginFlowResult(result);
    await shubidu(loginResult, authContext);
  };
};

const initRecovery = () => {
  const recoverButton = document.getElementById(
    "recoverButton",
  ) as HTMLButtonElement;
  recoverButton.onclick = () => useRecovery(getUserNumber());
};

const setMode = (mode: "existingUserNumber" | "newUserNumber") => {
  const existingUserNumber = document.getElementById(
    "existingUserNumber",
  ) as HTMLElement;
  const newUserNumber = document.getElementById("newUserNumber") as HTMLElement;
  existingUserNumber.classList.toggle("hidden", mode !== "existingUserNumber");
  newUserNumber.classList.toggle("hidden", mode !== "newUserNumber");
};

const formatTimeToLive = (maxTimeToLive: bigint | undefined) => {
  const seconds =
    maxTimeToLive !== undefined
      ? Math.floor(Number(maxTimeToLive / BigInt(1_000_000_000)))
      : 30 * 60 * 60; // 30 minutes default
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
      "second",
    );
  }
  return formatTimeUnit(seconds, "second");
};

function formatWithSecondaryTimeUnit(
  primaryAmount: number,
  primaryUnit: string,
  secondaryAmount: number,
  secondaryUnit: string,
) {
  let result = formatTimeUnit(primaryAmount, primaryUnit);
  if (secondaryAmount > 0) {
    result = result + `, ${formatTimeUnit(secondaryAmount, secondaryUnit)}`;
  }
  return result;
}

const formatTimeUnit = (amount: number, unit: string) => {
  const roundedAmount = Math.floor(amount);
  return `${roundedAmount} ${unit}${roundedAmount > 1 ? "s" : ""}`;
};

const readUserNumber = () => {
  const newUserNumber = document.getElementById("newUserNumber") as HTMLElement;
  if (!newUserNumber.classList.contains("hidden")) {
    return parseUserNumber(
      (document.getElementById("userNumberInput") as HTMLInputElement).value,
    ) as bigint;
  }
  return getUserNumber() as bigint;
};
