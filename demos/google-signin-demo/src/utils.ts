import {
  Cbor,
  Certificate,
  type HashTree,
  hashValue,
  lookup_path,
  LookupPathStatus,
  reconstruct,
  unwrapDER,
} from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { uint8Equals } from "@icp-sdk/core/candid";
import { z } from "zod";

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
): Promise<Principal> => {
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
    throw new Error(
      "Could not find certified data for this canister in the certificate",
    );
  }
  if (!uint8Equals(witness.value, reconstructed)) {
    throw new Error("Witness != Tree passed in ic-certification");
  }

  // Verify that message is within tree
  const lookupPath = lookup_path(
    ["sig", hashValue(seed), hashValue(params.message)],
    decodedSignature.tree,
  );
  if (lookupPath.status !== LookupPathStatus.Found) {
    throw new Error("Could not find message in tree");
  }

  // Return principal of canister that made the signature
  return canisterId;
};

export const AttributeResultCodec = z.codec(
  z.object({
    attributes: z.record(
      z.string(),
      z.object({
        value: z.string(),
        signature: z.base64(),
        expiration: z.string(),
      }),
    ),
  }),
  z.object({
    attributes: z.record(
      z.string(),
      z.object({
        value: z.string(),
        signature: z.instanceof(Uint8Array),
        expiration: z.bigint(),
      }),
    ),
  }),
  {
    decode: (json) => ({
      attributes: Object.fromEntries(
        Object.entries(json.attributes).map(
          ([key, { value, signature, expiration }]) => [
            key,
            {
              value,
              signature: z.util.base64ToUint8Array(signature),
              expiration: BigInt(expiration),
            },
          ],
        ),
      ),
    }),
    encode: (data) => ({
      attributes: Object.fromEntries(
        Object.entries(data.attributes).map(
          ([key, { value, signature, expiration }]) => [
            key,
            {
              value,
              signature: z.util.uint8ArrayToBase64(signature),
              expiration: expiration.toString(),
            },
          ],
        ),
      ),
    }),
  },
);
