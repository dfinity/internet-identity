import { html, render, TemplateResult } from "lit-html";
import { icLogo, caretDownIcon } from "../../components/icons";
import { footer } from "../../components/footer";
import { getUserNumber, setUserNumber } from "../../utils/userNumber";
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
import { fetchDelegation } from "./fetchDelegation";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";
import {
  validateDerivationOrigin,
  ValidationResult,
} from "./validateDerivationOrigin";
import { unreachable } from "../../utils/utils";
import { withRef } from "../../utils/lit-html";

type PageProps = {
  origin: string;
  onContinue: (arg: bigint) => void;
  recoverAnchor: (userNumner?: bigint) => void;
  register: () => void;
  userNumber?: bigint;
  derivationOrigin?: string;
};

type ChasmOpts = {
  info: string;
  message: TemplateResult;
};

const mkChasm = ({ info, message }: ChasmOpts): TemplateResult => {
  /* the chasm that opens to reveal details about alternative origin */
  const chasmRef: Ref<HTMLDivElement> = createRef();

  /* the (purely visual) arrow on the chasm */
  const chasmToggleRef: Ref<HTMLSpanElement> = createRef();

  /* Toggle the chasm open/closed */
  const chasmToggle = () =>
    withRef(chasmRef, (chasm) => {
      const classes = chasm.classList;

      if (classes.contains("c-chasm--closed")) {
        classes.remove("c-chasm--closed");
        classes.add("c-chasm--open");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.add("c-chasm__button--flipped")
        );
      } else if (classes.contains("c-chasm--open")) {
        classes.remove("c-chasm--open");
        classes.add("c-chasm--closed");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.remove("c-chasm__button--flipped")
        );
      }
    });

  return html`
    <p class="t-paragraph t-weak"><span id="alternative-origin-chasm-toggle" class="t-action" @click="${chasmToggle}" >${info} <span ${ref(
    chasmToggleRef
  )} class="t-link__icon c-chasm__button">${caretDownIcon}</span></span><br/>
      <div ${ref(chasmRef)} class="c-chasm c-chasm--closed">
        <div class="c-chasm__arrow"></div>
        <div class="t-weak c-chasm__content">
            ${message}
        </div>
      </div>
    </p>
`;
};

const mkLinks = ({
  recoverAnchor,
  register,
}: {
  recoverAnchor: () => void;
  register: () => void;
}) => html`
  <div class="l-stack">
    <ul class="c-list--flex">
      <li>
        <a @click=${register} id="registerButton" class="t-link"
          >Create Anchor</a
        >
      </li>
      <li>
        <a @click="${recoverAnchor}" id="recoverButton" class="t-link"
          >Lost Access?</a
        >
      </li>
    </ul>
    <ul class="c-list--flex">
      <li>
        <a href="/" class="t-link" id="manageButton">Home</a>
      </li>
      <li>
        <a class="t-link" href="/faq" target="_blank" rel="noopener noreferrer"
          >FAQ</a
        >
      </li>
    </ul>
  </div>
`;

const pageContent = ({
  origin,
  onContinue,
  recoverAnchor,
  register,
  userNumber,
  derivationOrigin,
}: PageProps): TemplateResult => {
  const anchorInput = mkAnchorInput({
    userNumber,
    onSubmit: onContinue,
  });

  return html` <div class="l-container c-card c-card--highlight">
      <!-- The title is hidden but used for accessibility -->
      <h1 class="is-hidden">Internet Identity</h1>
      <div class="c-logo">${icLogo}</div>
      <div class="l-stack t-centered">
        <p class="t-lead">
          Connect to<br />
          <span class="t-strong">${origin}</span>
          <br />
        </p>
        ${derivationOrigin !== undefined
          ? mkChasm({
              info: "shared identity",
              message: html`<span class="t-strong">${origin}</span> is an
                alternative domain of <br /><span class="t-strong"
                  >${derivationOrigin}</span
                ><br />and you will be authenticated to both with the same
                identity. `,
            })
          : ""}
      </div>

      ${anchorInput.template}

      <button
        @click="${anchorInput.submit}"
        id="authorizeButton"
        class="c-button"
      >
        Authorize
      </button>
      ${mkLinks({
        register,
        recoverAnchor: () => recoverAnchor(anchorInput.readUserNumber()),
      })}
    </div>
    ${footer}`;
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
      return authenticatePage(connection, authContext, userNumber);
    default:
      unreachable(validationResult);
      break;
  }
};

const authenticatePage = (
  connection: Connection,
  authContext: AuthContext,
  userNumber?: bigint
): Promise<AuthSuccess> => {
  return new Promise((resolve) => {
    displayPage({
      origin: authContext.requestOrigin,
      onContinue: async (userNumber) => {
        const authSuccess = await authenticateUser(
          connection,
          authContext,
          userNumber
        );
        resolve(authSuccess);
      },
      register: () =>
        registerIfAllowed(connection)
          .then((result) => {
            if (result === null) {
              // user canceled registration
              return authenticatePage(connection, authContext, userNumber);
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
            }).then(() =>
              authenticatePage(connection, authContext, userNumber)
            );
          })
          .then(resolve),

      recoverAnchor: (userNumber) => useRecovery(connection, userNumber),
      userNumber,
      derivationOrigin: authContext.authRequest.derivationOrigin,
    });
  });
};

const authenticateUser = async (
  connection: Connection,
  authContext: AuthContext,
  userNumber: bigint
): Promise<AuthSuccess> => {
  try {
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
  return authenticatePage(connection, authContext, userNumber);
};

export const displayPage = (props: PageProps): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(props), container);
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
