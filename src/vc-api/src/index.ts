import { Principal } from "@dfinity/principal";
import { isNullish } from "@dfinity/utils";
import { z } from "zod";
import { ArgumentValue } from "./generated/vc_issuer_types";

export type * from "./generated/vc_issuer_types";

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
      z.record(z.string(), z.union([z.string(), z.number()])),
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
  arg: Record<string, string | number>,
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
      canisterId: zodPrincipal,
    }),
    credentialSpec: zodCredentialSpec,
    credentialSubject: zodPrincipal,
    derivationOrigin: z.optional(z.string()),
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
