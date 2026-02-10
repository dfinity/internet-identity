import { z } from "zod";
import { Delegation, DelegationChain } from "@icp-sdk/core/identity";
import { type Signature } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";

export const INVALID_PARAMS_ERROR_CODE = -32602;

export interface ChannelOptions {
  allowedOrigin?: string;
  pending?: boolean;
}

export interface Channel {
  origin: string;
  closed: boolean;
  addEventListener(event: "close", listener: () => void): () => void;
  addEventListener(
    event: "request",
    listener: (request: JsonRequest) => void,
  ): () => void;
  send(response: JsonResponse): Promise<void>;
  close(): Promise<void>;
}

export interface Transport {
  establishChannel(options?: ChannelOptions): Promise<Channel>;
}

export type JsonArray = JsonValue[];

export type JsonObject = { [key: string]: JsonValue };

export type JsonValue =
  | boolean
  | string
  | number
  | null
  | JsonArray
  | JsonObject
  // Not actually supported in JSON, but it is in PostMessage,
  // so it's added here for broader compatibility with clients.
  | undefined;

export const JsonArraySchema: z.ZodType<JsonArray> = z.lazy(() =>
  z.array(JsonValueSchema),
);

export const JsonObjectSchema: z.ZodType<JsonObject> = z.lazy(() =>
  z.record(z.string(), JsonValueSchema),
);

export const JsonValueSchema: z.ZodType<JsonValue> = z.lazy(() =>
  z.union([
    z.boolean(),
    z.string(),
    z.number(),
    z.undefined(),
    JsonArraySchema,
    JsonObjectSchema,
  ]),
);

export const JsonRequestSchema = z.object({
  jsonrpc: z.literal("2.0"),
  id: z.optional(z.union([z.string(), z.number()])),
  method: z.string(),
  params: z.optional(z.union([JsonArraySchema, JsonObjectSchema])),
});

export type JsonRequest = z.infer<typeof JsonRequestSchema>;

export const JsonResponseSchema = z
  .object({
    jsonrpc: z.literal("2.0"),
    id: z.union([z.string(), z.number()]),
  })
  .and(
    z.union([
      z.object({ result: JsonValueSchema }),
      z.object({
        error: z.object({
          code: z.number(),
          message: z.string(),
          data: z.optional(JsonValueSchema),
        }),
      }),
    ]),
  );

export type JsonResponse = z.infer<typeof JsonResponseSchema>;

export const Base64ToBytesCodec = z.codec(
  z.base64(),
  z.instanceof(Uint8Array),
  {
    decode: (base64String) => z.util.base64ToUint8Array(base64String),
    encode: (bytes) => z.util.uint8ArrayToBase64(bytes),
  },
);

export const Base64ToPublicKeyCodec = z.codec(
  z.base64(),
  z.object({
    toDer: z.function({ input: [], output: z.instanceof(Uint8Array) }),
  }),
  {
    decode: (base64String) => ({
      toDer: () => z.util.base64ToUint8Array(base64String),
    }),
    encode: (publicKey) => z.util.uint8ArrayToBase64(publicKey.toDer()),
  },
);

export const StringToBigIntCodec = z.codec(z.string(), z.bigint(), {
  decode: (str) => BigInt(str),
  encode: (bigint) => bigint.toString(),
});

export const StringOrNumberToBigIntCodec = z.codec(
  z.union([z.string(), z.number(), z.bigint()]),
  z.bigint(),
  {
    decode: (value) => BigInt(value),
    encode: (bigint) => bigint.toString(),
  },
);

export const OriginSchema = z.string().refine(
  (value) => {
    try {
      const url = new URL(value);
      return url.origin === value;
    } catch {
      return false;
    }
  },
  {
    message: "Invalid origin",
  },
);

export const DelegationParamsCodec = z.object({
  publicKey: Base64ToPublicKeyCodec,
  maxTimeToLive: z.optional(StringToBigIntCodec),
  icrc95DerivationOrigin: z.optional(OriginSchema),
});

export type DelegationParams = z.infer<typeof DelegationParamsCodec>;

export const DelegationResultSchema = z.codec(
  z.object({
    publicKey: z.base64(),
    signerDelegation: z.array(
      z.object({
        delegation: z.object({
          pubkey: z.base64(),
          expiration: z.string(),
        }),
        signature: z.base64(),
      }),
    ),
  }),
  z.custom<DelegationChain>((arg) => arg instanceof DelegationChain),
  {
    decode: ({ publicKey, signerDelegation }) =>
      DelegationChain.fromDelegations(
        signerDelegation.map(
          ({ delegation: { pubkey, expiration }, signature }) => ({
            delegation: new Delegation(
              Base64ToBytesCodec.decode(pubkey),
              StringToBigIntCodec.decode(expiration),
            ),
            signature: Base64ToBytesCodec.decode(signature) as Signature,
          }),
        ),
        Base64ToBytesCodec.decode(publicKey),
      ),
    encode: (delegationChain) => ({
      publicKey: Base64ToBytesCodec.encode(
        new Uint8Array(delegationChain.publicKey),
      ),
      signerDelegation: delegationChain.delegations.map((delegation) => ({
        delegation: {
          pubkey: Base64ToBytesCodec.encode(
            new Uint8Array(delegation.delegation.pubkey),
          ),
          expiration: delegation.delegation.expiration.toString(),
        },
        signature: Base64ToBytesCodec.encode(
          new Uint8Array(delegation.signature),
        ),
      })),
    }),
  },
);

// Legacy auth request message
export const AuthRequestCodec = z.codec(
  z.object({
    kind: z.literal("authorize-client"),
    sessionPublicKey: z.union([z.instanceof(Uint8Array), z.base64()]),
    maxTimeToLive: z.optional(z.union([z.string(), z.number(), z.bigint()])),
    derivationOrigin: z.optional(z.lazy(() => OriginSchema)),
    allowPinAuthentication: z.optional(z.boolean()),
    autoSelectionPrincipal: z.optional(z.string()),
  }),
  z.object({
    kind: z.literal("authorize-client"),
    sessionPublicKey: z.instanceof(Uint8Array),
    maxTimeToLive: z.optional(z.bigint()),
    derivationOrigin: z.optional(z.lazy(() => OriginSchema)),
    allowPinAuthentication: z.optional(z.boolean()),
    autoSelectionPrincipal: z.optional(
      z.custom<Principal>((arg) => arg instanceof Principal),
    ),
  }),
  {
    decode: ({
      kind,
      sessionPublicKey,
      maxTimeToLive,
      derivationOrigin,
      allowPinAuthentication,
      autoSelectionPrincipal,
    }) => ({
      kind,
      sessionPublicKey:
        typeof sessionPublicKey === "string"
          ? z.util.base64ToUint8Array(sessionPublicKey)
          : sessionPublicKey,
      maxTimeToLive:
        maxTimeToLive !== undefined ? BigInt(maxTimeToLive) : undefined,
      derivationOrigin,
      allowPinAuthentication,
      autoSelectionPrincipal:
        autoSelectionPrincipal !== undefined
          ? Principal.fromText(autoSelectionPrincipal)
          : undefined,
    }),
    encode: ({
      kind,
      sessionPublicKey,
      maxTimeToLive,
      derivationOrigin,
      allowPinAuthentication,
      autoSelectionPrincipal,
    }) => ({
      kind,
      sessionPublicKey: z.util.uint8ArrayToBase64(sessionPublicKey),
      maxTimeToLive: maxTimeToLive?.toString(),
      derivationOrigin,
      allowPinAuthentication,
      autoSelectionPrincipal: autoSelectionPrincipal?.toText(),
    }),
  },
);

export type AuthRequest = z.output<typeof AuthRequestCodec>;

// Legacy auth response message
export const AuthResponseCodec = z.codec(
  z.union([
    z.object({
      kind: z.literal("authorize-client-success"),
      delegations: z.array(
        z.object({
          delegation: z.union([
            z.object({
              pubkey: z.base64(),
              expiration: z.string(),
              targets: z.optional(z.array(z.string())),
            }),
            z.instanceof(Delegation),
          ]),
          signature: z.union([z.base64(), z.instanceof(Uint8Array)]),
        }),
      ),
      userPublicKey: z.union([z.base64(), z.instanceof(Uint8Array)]),
      authnMethod: z.union([
        z.literal("pin"),
        z.literal("passkey"),
        z.literal("recovery"),
      ]),
    }),
    z.object({
      kind: z.literal("authorize-client-failure"),
      text: z.string(),
    }),
  ]),
  z.union([
    z.object({
      kind: z.literal("authorize-client-success"),
      delegations: z.array(
        z.object({
          delegation: z.instanceof(Delegation),
          signature: z.instanceof(Uint8Array),
        }),
      ),
      userPublicKey: z.instanceof(Uint8Array),
      authnMethod: z.union([
        z.literal("pin"),
        z.literal("passkey"),
        z.literal("recovery"),
      ]),
    }),
    z.object({
      kind: z.literal("authorize-client-failure"),
      text: z.string(),
    }),
  ]),
  {
    decode: (response) =>
      response.kind === "authorize-client-success"
        ? {
            kind: response.kind,
            delegations: response.delegations.map(
              ({ delegation, signature }) => ({
                delegation:
                  delegation instanceof Delegation
                    ? delegation
                    : new Delegation(
                        z.util.base64ToUint8Array(delegation.pubkey),
                        BigInt(delegation.expiration),
                        delegation.targets?.map((target) =>
                          Principal.fromText(target),
                        ),
                      ),
                signature:
                  typeof signature === "string"
                    ? z.util.base64ToUint8Array(signature)
                    : signature,
              }),
            ),
            userPublicKey:
              typeof response.userPublicKey === "string"
                ? z.util.base64ToUint8Array(response.userPublicKey)
                : response.userPublicKey,
            authnMethod: response.authnMethod,
          }
        : response,
    encode: (response) =>
      response.kind === "authorize-client-success"
        ? {
            kind: response.kind,
            delegations: response.delegations.map(
              ({ delegation, signature }) => ({
                delegation: {
                  pubkey: z.util.uint8ArrayToBase64(delegation.pubkey),
                  expiration: delegation.expiration.toString(),
                  targets: delegation.targets?.map((target) => target.toText()),
                },
                signature: z.util.uint8ArrayToBase64(signature),
              }),
            ),
            userPublicKey: z.util.uint8ArrayToBase64(response.userPublicKey),
            authnMethod: response.authnMethod,
          }
        : response,
  },
);

export type AuthResponse = z.output<typeof AuthResponseCodec>;

export const AttributesParamsSchema = z.object({
  attributes: z.array(z.string()),
});
