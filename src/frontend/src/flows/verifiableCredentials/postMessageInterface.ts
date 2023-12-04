import { ArgumentValue } from "$generated/vc_issuer_types";
import { toast } from "$src/components/toast";
import { Principal } from "@dfinity/principal";
import { isNullish } from "@dfinity/utils";
import { z } from "zod";

// The type of messages that kick start the flow (II -> RP)
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

// Parse & validate a CredentialSpec
const zodCredentialSpec = z
  /* The input object */
  .object({
    credentialType: z.string(),
    arguments: z.optional(
      z.record(z.string(), z.union([z.string(), z.number()]))
    ),
  })
  /* Transform to make the type easier to use:
   *    - transform the arguments to whatever the did spec expects
   *    - work around the JS type gen weirdness for optionals
   * XXX: TS needs the type annotation or it gets slightly confused
   */
  .transform<{
    credential_type: string;
    arguments: [] | [Array<[string, ArgumentValue]>];
  }>(({ credentialType, arguments: args }) => ({
    credential_type: credentialType,
    arguments: isNullish(args) ? [] : [fixupArgs(args)],
  }));

/* Convert the JSON map/record into what the did spec expects */
const fixupArgs = (
  arg: Record<string, string | number>
): Array<[string, ArgumentValue]> => {
  return Object.entries(arg).map(([k, v]) => [
    k,
    typeof v === "string" ? { String: v } : { Int: v },
  ]);
};

// The request (RP -> II)
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
      origin: z
        .string()
        .url() /* XXX: we limit to URLs, but in practice should even be an origin */,
      canisterId: z.optional(zodPrincipal),
    }),
    credentialSpec: zodCredentialSpec,
    credentialSubject: zodPrincipal,
  }),
});

// The wire format of a VC flow request
export type VcFlowRequestWire = z.input<typeof VcFlowRequest>;
export type VcFlowRequest = z.output<typeof VcFlowRequest>;

// The final response concluding the flow (II -> RP)
export type VcResponse = {
  id: VcFlowRequest["id"];
  jsonrpc: "2.0";
} & (
  | { result: VcVerifiablePresentation }
  | { error: { version: "1"; code: "UNKNOWN" } }
);

export type VcVerifiablePresentation = {
  verifiablePresentation: string;
};

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
    origin
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
