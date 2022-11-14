import { html, render } from "lit-html";
import { unreachable } from "../../utils/utils";
import { Connection } from "../../utils/iiConnection";
import { displayError } from "../../components/displayError";
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

  return { message, chasm };
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
