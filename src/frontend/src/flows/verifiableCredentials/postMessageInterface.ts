import { toast } from "$src/components/toast";
import { Principal } from "@dfinity/principal";
import { z } from "zod";

export const VcFlowReady = {
  jsonrpc: "2.0",
  method: "vc-flow-ready",
};

const zodPrincipal = z.string().transform((val, ctx) => {
  let principal;
  try {
    principal = Principal.fromText(val);
  } catch {
    ctx.addIssue({ code: z.ZodIssueCode.custom, message: "Not a principal " });
    return z.NEVER;
  }

  return principal;
});

// https://www.jsonrpc.org/specification
// https://github.com/dfinity/internet-identity/blob/vc-mvp/docs/vc-spec.md#identity-provider-api
export const VcFlowRequest = z.object({
  id: z.union([
    z.number(),
    z.string(),
  ]) /* Slightly lax; in principle jsonrpc does not allow fractional numbers as id */,
  jsonrpc: z.literal("2.0"),
  method: z.literal("request_credential"),
  params: z.object({
    issuer: z.object({
      issuerOrigin: z
        .string()
        .url() /* XXX: we limit to URLs, but in practice should even be an origin */,
      credentialId: z.string(),
    }),
    credentialSubject: zodPrincipal,
  }),
});

// The wire format of a VC flow request
export type VcFlowRequestWire = z.input<typeof VcFlowRequest>;
export type VcFlowRequest = z.output<typeof VcFlowRequest>;

export type VcVerifiablePresentation = {
  id: VcFlowRequest["id"];
  jsonrpc: "2.0";
  result: {
    verifiablePresentation: string;
  };
};

export const vcProtocol = async ({
  onProgress,
  verifyCredentials,
}: {
  onProgress: (state: "waiting" | "verifying") => void;
  verifyCredentials: (args: {
    request: VcFlowRequest["params"];
    rpOrigin: string;
  }) => Promise<VcVerifiablePresentation["result"]>;
}) => {
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

  const { origin, request } = await waitForRequest();
  const reqId = request.id;

  onProgress("verifying");

  const result = await verifyCredentials({
    request: request.params,
    rpOrigin: origin,
  });

  window.opener.postMessage(
    {
      id: reqId,
      jsonrpc: "2.0",
      result,
    } satisfies VcVerifiablePresentation,
    origin
  );
};

const waitForRequest = (): Promise<{
  request: VcFlowRequest;
  origin: string;
}> => {
  return new Promise((resolve) => {
    const messageEventHandler = (evnt: MessageEvent) => {
      const message: unknown = evnt.data;
      const result = VcFlowRequest.safeParse(message);

      if (!result.success) {
        const message = `Unexpected error: flow request ` + result.error;
        console.error(message);
        toast.error(message);
        return; // XXX: this just waits further; correct?
      }

      window.removeEventListener("message", messageEventHandler);

      resolve({ request: result.data, origin: evnt.origin });
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", messageEventHandler);
  });
};
