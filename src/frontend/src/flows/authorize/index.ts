import { html, render, TemplateResult } from "lit-html";
import { withRef } from "../../utils/lit-html";
import { caretDownIcon } from "../../components/icons";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import { unreachable } from "../../utils/utils";
import { Connection } from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { spinner } from "../../components/icons";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { authenticationProtocol } from "./postMessageInterface";
import {
  authenticateBox,
  AuthnTemplates,
} from "../../components/authenticateBox";

/* Template for the authbox when authenticating to a dapp */
export const authnTemplateAuthorize = ({
  origin,
  derivationOrigin,
}: {
  origin: string;
  derivationOrigin?: string;
}): AuthnTemplates => {
  const chasm =
    derivationOrigin !== undefined
      ? mkChasm({
          info: "shared identity",
          message: html`<span class="t-strong">${origin}</span> is an
            alternative domain of <br /><span class="t-strong"
              >${derivationOrigin}</span
            ><br />and you will be authenticated to both with the same identity.`,
        })
      : undefined;

  const wrap = (slot: string) => html`
    <div class="l-stack t-centered">
      <p class="t-lead">
        ${slot}<br />
        <span class="t-strong">${origin}</span><br />
      </p>
      ${chasm}
    </div>
  `;
  return {
    new: {
      slot: wrap("Create an anchor to continue to"),
      useExistingText: "Use Existing",
    },
    useExisting: {
      slot: wrap("Enter your anchor to continue to"),
    },

    pick: {
      slot: wrap("Choose an anchor to continue to"),
    },
  };
};

/** Run the authentication flow, including postMessage protocol, offering to authenticate
 * using an existing anchor or creating a new anchor, etc.
 */
export const authFlowAuthorize = async (
  connection: Connection
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(html`<h1>starting authentication</h1>`, container);
  const showMessage = (msg: string) =>
    render(
      html`
        <div class="l-container c-card c-card--highlight t-centered">
          <div class="c-spinner">${spinner}</div>
          <p class="t-lead t-paragraph l-stack">${msg}</p>
        </div>
      `,
      container
    );
  const result = await authenticationProtocol({
    authenticate: async (authContext) => {
      const authSuccess = await authenticateBox(
        connection,
        authnTemplateAuthorize({
          origin: authContext.requestOrigin,
          derivationOrigin: authContext.authRequest.derivationOrigin,
        })
      );
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
          showMessage("Waiting for authentication data...");
          break;
        case "validating":
          showMessage("validating authentication data...");
          break;
        case "fetching delegation":
          showMessage("Finalizing authentication...");
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
        html`<h1
          style="position: absolute; max-width: 100%; top: 50%; transform: translate(0, -50%);"
          data-role="notify-auth-success"
        >
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

/** Options to display a "chasm" in the authbox */
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
      const expanded = chasm.getAttribute("aria-expanded") === "true";
      if (!expanded) {
        chasm.setAttribute("aria-expanded", "true");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.add("c-chasm__button--flipped")
        );
      } else {
        chasm.setAttribute("aria-expanded", "false");

        withRef(chasmToggleRef, (arrow) =>
          arrow.classList.remove("c-chasm__button--flipped")
        );
      }
    });

  return html`
    <p class="t-paragraph t-weak"><span id="alternative-origin-chasm-toggle" class="t-action" @click="${chasmToggle}" >${info} <span ${ref(
    chasmToggleRef
  )} class="t-link__icon c-chasm__button">${caretDownIcon}</span></span>
      <div ${ref(chasmRef)} class="c-chasm" aria-expanded="false">
        <div class="c-chasm__arrow"></div>
        <div class="t-weak c-chasm__content">
            ${message}
        </div>
      </div>
    </p>
`;
};
