import { html, render, TemplateResult } from "lit-html";
import { icLogo, caretDownIcon } from "../../components/icons";
import { footer } from "../../components/footer";
import { getUserNumber, setUserNumber } from "../../utils/userNumber";
import { unreachable } from "../../utils/utils";
import { withLoader } from "../../components/loader";
import { mkAnchorInput } from "../../components/anchorInput";
import { Connection } from "../../utils/iiConnection";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
  LoginFlowSuccess,
  LoginFlowError,
  LoginData,
} from "../login/flowResult";
import { displayError } from "../../components/displayError";
import { useRecovery } from "../recovery/useRecovery";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { AuthContext, authenticationProtocol } from "./postMessageInterface";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";
import { withRef } from "../../utils/lit-html";
import { mkAnchorPicker } from "../../components/anchorPicker";

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
    <span
      id="alternative-origin-chasm-toggle"
      class="t-action"
      @click="${chasmToggle}"
      >${info}
      <span ${ref(chasmToggleRef)} class="t-link__icon c-chasm__button"
        >⚠️</span
      ></span
    ><br />
    <div ${ref(chasmRef)} class="c-chasm c-chasm--closed">
      <div class="c-chasm__arrow"></div>
      <div class="t-weak c-chasm__content">${message}</div>
    </div>
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

  return html`<div class="c-card c-card--background">
    <div class="c-logo">${icLogo}</div>
    <div class="l-container c-card c-card--highlight">
      <!-- The title is hidden but used for accessibility -->
      <h1 class="is-hidden">Internet Identity</h1>
      <div class="t-centered">
        <div class="c-pill">
          <span>Connect to</span>
          <strong class="t-strong">${origin}</strong>
          ${derivationOrigin !== undefined
            ? mkChasm({
                info: "",
                message: html`<span class="t-strong">${origin}</span> is an
                  alternative domain of <br /><span class="t-strong"
                    >${derivationOrigin}</span
                  ><br />and you will be authenticated to both with the same
                  identity. `,
              })
            : ""}
        </div>
      </div>

      ${mkAnchorPicker({
        savedAnchors: [BigInt(1231235), BigInt(78345987)],
        pick: (anchor) => {
          anchorInput.submit;
        },
      }).template}

      <!--button
        @click="${anchorInput.submit}"
        id="authorizeButton"
        class="c-button"
      >
        Authorize
      </button-->
      <!--
      ${mkLinks({
        register,
        recoverAnchor: () => recoverAnchor(anchorInput.readUserNumber()),
      })} -->
    </div>
    ${footer}
  </div>`;
};

export const authenticatePage = (
  connection: Connection,
  authContext: AuthContext
): Promise<LoginData> => {
  const retryOnError = async (result: LoginFlowResult): Promise<LoginData> => {
    switch (result.tag) {
      case "err":
        await displayError({
          title: result.title,
          message: result.message,
          detail: result.detail !== "" ? result.detail : undefined,
          primaryButton: "Try again",
        });
        return authenticatePage(connection, authContext);
      case "ok":
        return {
          userNumber: result.userNumber,
          connection: result.connection,
        };
      case "canceled":
        return authenticatePage(connection, authContext);
      default:
        unreachable(result);
        break;
    }
  };

  return new Promise<LoginData>((resolve) => {
    displayPage({
      origin: authContext.requestOrigin,
      onContinue: async (userNumber) => {
        const loginData = await authenticate(connection, userNumber).then(
          retryOnError
        );
        setUserNumber(loginData.userNumber);
        resolve(loginData);
      },
      register: async () => {
        const loginData = await registerIfAllowed(connection).then(
          retryOnError
        );
        setUserNumber(loginData.userNumber);
        resolve(loginData);
      },
      recoverAnchor: (userNumber) => useRecovery(connection, userNumber),
      userNumber: getUserNumber(),
      derivationOrigin: authContext.authRequest.derivationOrigin,
    });
  });
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

export const displayPage = (props: PageProps): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(props), container);
};

/** Run the authentication flow, including postMessage protocol, offering to authenticate
 * using an existing anchor or creating a new anchor, etc.
 */
export const authenticationFlow = async (
  connection: Connection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(html`<h1>starting authentication</h1>`, container);
  const result = await authenticationProtocol({
    authenticate: async (authContext) => {
      const authSuccess = await authenticatePage(connection, authContext);
      await recoveryWizard(authSuccess.userNumber, authSuccess.connection);
      return authSuccess;
    },
    onInvalidOrigin: (result) =>
      displayError({
        title: "Invalid Derivation Origin",
        message: `"${result.authContext.authRequest.derivationOrigin}" is not a valid derivation origin for "${result.authContext.requestOrigin}"`,
        detail: result.message,
        primaryButton: "Continue",
      }),

    onProgress: (status) => {
      switch (status) {
        case "waiting":
          render(
            html`<h1>waiting for authentication data from service...</h1>`,
            container
          );
          break;
        case "validating":
          render(html`<h1>validating authentication data...</h1>`, container);
          break;
        case "fetching delegation":
          render(html`<h1>fetching delegation...</h1>`, container);
          break;
        default:
          unreachable(status);
          break;
      }
    },
  });

  switch (result) {
    case "orphan":
      await displayError({
        title: "Invalid Data",
        message: `It looks like you were sent here for authentication, but no service requested authentication.`,
        primaryButton: "Home",
      });

      location.hash = "";
      window.location.reload();
      break;
    case "failure":
      render(
        html`<h1>
          Something went wrong during authentication. Authenticating service was
          notified and you may close this page.
        </h1>`,
        container
      );
      break;
    case "success":
      render(
        html`<h1 data-role="notify-auth-success">
          Authentication successful. You may close this page.
        </h1>`,
        container
      );
      break;
    default:
      unreachable(result);
      break;
  }
};
