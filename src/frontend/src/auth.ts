import { UserNumber } from "../generated/internet_identity_types";
import { IIConnection } from "./utils/iiConnection";
import { handleAuthRequest } from "./flows/authenticate/fetchDelegation";
import { READY_MESSAGE } from "./flows/authenticate/postMessageInterface";

/**
 * Setup an event listener to listen to authorize requests from the client.
 *
 * This method expects to be called after the login flow.
 */
export default async function setup(
  userNumber: UserNumber,
  connection: IIConnection
): Promise<void> {
  // Set up an event listener for receiving messages from the client.
  window.addEventListener("message", async (event) => {
    const message = event.data;
    if (message.kind === "authorize-client") {
      console.log("Handling authorize-client request.");
      const response = await handleAuthRequest(
        connection,
        userNumber,
        message,
        event.origin
      );
      (event.source as Window).postMessage(response, event.origin);
    } else {
      console.log(`Message of unknown kind received: ${message}`);
    }
  });

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  if (window.opener !== null) {
    window.opener.postMessage(READY_MESSAGE, "*");
  } else {
    // If there's no `window.opener` a user has manually navigated to "/#authorize". Let's
    // redirect them to the non-hash version.
    window.location.hash = "";
    window.location.reload();
  }
}
