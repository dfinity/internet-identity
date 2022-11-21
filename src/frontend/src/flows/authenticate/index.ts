import { html, render } from "lit-html";
import { unreachable } from "../../utils/utils";
import { Connection } from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
import { spinner } from "../../components/icons";
import { recoveryWizard } from "../recovery/recoveryWizard";
import { authenticationProtocol } from "./postMessageInterface";
import {
  authenticateBox,
  AuthTemplates,
} from "../../components/authenticateBox";

/* Template for the authbox when authenticating to a dapp */
export const mkAuthTemplates = ({
  origin,
  derivationOrigin,
}: {
  origin: string;
  derivationOrigin?: string;
}): AuthTemplates => {
  const message = html`<p class="t-lead">
    Connect to<br />
    <span class="t-strong">${origin}</span><br />
  </p>`;
  const chasm =
    derivationOrigin !== undefined
      ? {
          info: "shared identity",
          message: html`<span class="t-strong">${origin}</span> is an
            alternative domain of <br /><span class="t-strong"
              >${derivationOrigin}</span
            ><br />and you will be authenticated to both with the same identity.`,
        }
      : undefined;

  return { message, chasm, button: "Authorize" };
};

/** Run the authentication flow, including postMessage protocol, offering to authenticate
 * using an existing anchor or creating a new anchor, etc.
 */
export const authenticationFlow = async (
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
        mkAuthTemplates({
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
