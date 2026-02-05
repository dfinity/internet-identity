import {
  Cbor,
  Certificate,
  domain_sep,
  hashOfMap,
  type HashTree,
  hashValue,
  IC_ROOT_KEY,
  lookup_path,
  LookupPathStatus,
  reconstruct,
  unwrapDER,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { concat, uint8Equals } from "@icp-sdk/core/candid";
import { z } from "zod";
import { DelegationChain } from "@icp-sdk/core/identity";
import { Signer } from "@slide-computer/signer";
import { PostMessageTransport } from "@slide-computer/signer-web";

const CANISTER_SIGNATURE_OID = Uint8Array.from([
  ...[0x30, 0x0c], // SEQUENCE
  ...[0x06, 0x0a], // OID with 10 bytes
  ...[0x2b, 0x06, 0x01, 0x04, 0x01, 0x83, 0xb8, 0x43, 0x01, 0x02], // OID DFINITY
]);

interface VerifyCanisterSignatureParams {
  rootKey: Uint8Array;
  publicKey: Uint8Array;
  message: Uint8Array;
  signature: Uint8Array;
}
export const verifyCanisterSignature = async (
  params: VerifyCanisterSignatureParams,
): Promise<Principal | undefined> => {
  // Decode signature into certificate and tree
  const decodedSignature = Cbor.decode<{
    certificate: Uint8Array;
    tree: HashTree;
  }>(params.signature);

  // Split public key into canister id and seed
  const rawKey = unwrapDER(params.publicKey, CANISTER_SIGNATURE_OID);
  const canisterId = Principal.fromUint8Array(rawKey.slice(1, 1 + rawKey[0]));
  const seed = rawKey.slice(1 + rawKey[0]);

  // Create certificate instance, this also verifies it's BLS signature
  const certificate = await Certificate.create({
    certificate: decodedSignature.certificate,
    rootKey: params.rootKey,
    principal: { canisterId },
    disableTimeVerification: true, // Verify expiration in message instead
  });

  // Reconstruct certified data from tree and compare it with certificate
  const reconstructed = await reconstruct(decodedSignature.tree);
  const witness = certificate.lookup_path([
    "canister",
    canisterId.toUint8Array(),
    "certified_data",
  ]);
  if (witness.status !== LookupPathStatus.Found) {
    return;
  }
  if (!uint8Equals(witness.value, reconstructed)) {
    return;
  }

  // Verify that message is within tree
  const lookupPath = lookup_path(
    ["sig", hashValue(seed), hashValue(params.message)],
    decodedSignature.tree,
  );
  if (lookupPath.status !== LookupPathStatus.Found) {
    return;
  }

  // Return principal of canister that made the signature
  return canisterId;
};

const JsonnableCertifiedAttributesSchema = z.record(
  z.string(),
  z.object({
    value: z.base64(),
    signature: z.base64(),
    expiration: z.string(),
  }),
);

const CertifiedAttributesSchema = z.record(
  z.string(),
  z.object({
    value: z.instanceof(Uint8Array),
    signature: z.instanceof(Uint8Array),
    expiration: z.bigint(),
  }),
);

export type CertifiedAttributes = z.infer<typeof CertifiedAttributesSchema>;

export const JsonToCertifiedAttributesCodec = z.codec(
  JsonnableCertifiedAttributesSchema,
  CertifiedAttributesSchema,
  {
    decode: (json) =>
      Object.fromEntries(
        Object.entries(json).map(([key, { value, signature, expiration }]) => [
          key,
          {
            value: z.util.base64ToUint8Array(value),
            signature: z.util.base64ToUint8Array(signature),
            expiration: BigInt(expiration),
          },
        ]),
      ),
    encode: (data) =>
      Object.fromEntries(
        Object.entries(data).map(([key, { value, signature, expiration }]) => [
          key,
          {
            value: z.util.uint8ArrayToBase64(value),
            signature: z.util.uint8ArrayToBase64(signature),
            expiration: expiration.toString(),
          },
        ]),
      ),
  },
);

interface AuthenticationData {
  delegationChain: DelegationChain;
  certifiedAttributes: CertifiedAttributes;
}

interface AuthenticateParams {
  identityProvider: string;
  sessionPublicKey: Uint8Array;
  directOpenIdAuth?: string;
  attributes?: string[];
}

export const authenticate = async (
  params: AuthenticateParams,
): Promise<AuthenticationData> => {
  // Create ICRC-29 PostMessage transport
  const authorizeURL = new URL(params.identityProvider);
  authorizeURL.pathname = "/authorize";
  if (params.directOpenIdAuth !== undefined) {
    authorizeURL.searchParams.set("openid", params.directOpenIdAuth);
  }
  const transport = new PostMessageTransport({ url: authorizeURL.href });
  const signer = new Signer({ transport });

  // Send ICRC-34 delegation request
  const delegationPromise = signer.delegation({
    publicKey: params.sessionPublicKey,
  });

  // Send II attributes request
  const requestId = window.crypto.randomUUID();
  const attributesPromise: Promise<CertifiedAttributes> =
    params.attributes !== undefined && params.attributes.length > 0
      ? signer
          .sendRequest({
            jsonrpc: "2.0",
            method: "ii_attributes",
            id: requestId,
            params: {
              attributes: params.attributes,
            },
          })
          .then((response) => {
            if (
              !("result" in response) ||
              typeof response.result !== "object" ||
              response.result === null ||
              !("attributes" in response.result)
            ) {
              throw new Error("Attributes response is missing result");
            }
            return JsonToCertifiedAttributesCodec.parse(
              response.result.attributes,
            );
          })
      : Promise.resolve({});

  // Wait for both requests to receive a response
  const [delegationChain, certifiedAttributes] = await Promise.all([
    delegationPromise,
    attributesPromise,
  ]);
  return { delegationChain, certifiedAttributes };
};

export const verify = async (
  params: AuthenticationData & {
    sessionPublicKey: Uint8Array;
    canisterId: Principal;
  },
): Promise<boolean> => {
  // Get IC root key from agent-js
  const rootKey = z.util.hexToUint8Array(IC_ROOT_KEY);

  // Verify delegation chain
  const nowNanos = BigInt(Date.now()) * BigInt(1_000_000);
  if (params.delegationChain.delegations.length !== 1) {
    return false;
  }
  const signedDelegation = params.delegationChain.delegations[0];
  if (
    !uint8Equals(signedDelegation.delegation.pubkey, params.sessionPublicKey) ||
    signedDelegation.delegation.expiration < nowNanos ||
    signedDelegation.delegation.targets !== undefined
  ) {
    return false;
  }
  const delegationCanisterId = await verifyCanisterSignature({
    rootKey,
    publicKey: params.delegationChain.publicKey,
    message: concat(
      domain_sep("ic-request-auth-delegation"),
      hashOfMap({
        pubkey: signedDelegation.delegation.pubkey,
        expiration: signedDelegation.delegation.expiration,
      }),
    ),
    signature: signedDelegation.signature,
  });
  if (
    delegationCanisterId === undefined ||
    delegationCanisterId.toText() !== params.canisterId.toText()
  ) {
    return false;
  }

  // Verify all attributes one by one
  for (let [key, { value, signature, expiration }] of Object.entries(
    params.certifiedAttributes,
  )) {
    if (expiration < nowNanos) {
      return false;
    }
    const attributeCanisterId = await verifyCanisterSignature({
      rootKey,
      publicKey: params.delegationChain.publicKey,
      message: concat(
        domain_sep("ii-request-attribute"),
        hashOfMap({ [key]: value, expiration }),
      ),
      signature,
    });
    if (
      attributeCanisterId === undefined ||
      attributeCanisterId.toText() !== params.canisterId.toText()
    ) {
      return false;
    }
  }

  return true;
};
