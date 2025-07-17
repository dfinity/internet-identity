import { toast } from "$lib/templates/toast";
import {
  VcFlowReady,
  VcFlowRequest,
  VcResponse,
  VcVerifiablePresentation,
} from "@dfinity/internet-identity-vc-api";

export type { VcVerifiablePresentation } from "@dfinity/internet-identity-vc-api";

// The protocol, from a "postMessage" req/resp point of view
export const vcProtocol = async ({
  onProgress,
  verifyCredentials,
}: {
  onProgress: (state: "waiting" | "verifying") => void;
  verifyCredentials: (args: {
    request: VcFlowRequest["params"];
    rpOrigin: string;
  }) => Promise<VcVerifiablePresentation | "aborted">;
}): Promise<"orphan" | "success"> => {
  if (window.opener === null) {
    // If there's no `window.opener` a user has manually navigated to "/vc-flow".
    // Signal that there will never be an authentication request incoming.
    return "orphan";
  }

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  window.opener.postMessage(VcFlowReady, "*");

  onProgress("waiting");

  // Wait for the RP request
  const { origin, request } = await waitForRequest();
  const reqId = request.id;

  onProgress("verifying");

  // Verify the credentials
  const result = await verifyCredentials({
    request: request.params,
    rpOrigin: origin,
  });

  // Respond to the RP
  const content =
    result === "aborted"
      ? ({
          error: { version: "1", code: "UNKNOWN" },
        } as const)
      : { result };

  window.opener.postMessage(
    {
      id: reqId,
      jsonrpc: "2.0",
      ...content,
    } satisfies VcResponse,
    origin,
  );

  return "success";
};

// Wait for a request to kickstart the flow
const waitForRequest = (): Promise<{
  request: VcFlowRequest;
  origin: string;
}> => {
  return new Promise((resolve) => {
    const messageEventHandler = (evnt: MessageEvent) => {
      if (evnt.origin === window.location.origin) {
        // Ignore messages from own origin (e.g. from browser extensions)
        console.warn("Ignoring message from own origin", evnt);
        return;
      }
      const message: unknown = evnt.data;
      const result = VcFlowRequest.safeParse(message);

      if (!result.success) {
        const message = `Unexpected error: flow request ` + result.error;
        console.error(message);
        toast.error(message);
        // XXX: here we just wait further assuming the opener might recover
        // and send a correct request
        return;
      }

      window.removeEventListener("message", messageEventHandler);

      resolve({ request: result.data, origin: evnt.origin });
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", messageEventHandler);
  });
};
