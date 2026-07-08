import { describe, it, expect } from "vitest";
import { Delegation, DelegationChain } from "@icp-sdk/core/identity";
import type { Signature } from "@icp-sdk/core/agent";
import { DelegationResultSchema, AuthResponseCodec } from "./utils";

const PUBKEY = new Uint8Array(32).fill(2);
const USER_KEY = new Uint8Array(32).fill(3);
const EXPIRATION = BigInt(Date.now() + 3_600_000) * BigInt(1_000_000);
const SIGNATURE = new Uint8Array(64).fill(9) as unknown as Signature;

const chainWith = (permissions?: string): DelegationChain =>
  DelegationChain.fromDelegations(
    [
      {
        delegation: new Delegation(PUBKEY, EXPIRATION, undefined, permissions),
        signature: SIGNATURE,
      },
    ],
    USER_KEY,
  );

describe("DelegationResultSchema (ICRC-34 delegation forwarding)", () => {
  it("encodes the permissions field so the relying party receives it", () => {
    const encoded = DelegationResultSchema.encode(chainWith("queries"));

    expect(encoded.signerDelegation[0].delegation.permissions).toBe("queries");
  });

  it("round-trips a queries-only delegation (encode -> parse)", () => {
    const encoded = DelegationResultSchema.encode(chainWith("queries"));
    const chain = DelegationResultSchema.parse(encoded);

    expect(chain.delegations[0].delegation.permissions).toBe("queries");
  });

  it("omits permissions for an unrestricted delegation and round-trips cleanly", () => {
    const encoded = DelegationResultSchema.encode(chainWith(undefined));
    expect(encoded.signerDelegation[0].delegation.permissions).toBeUndefined();

    const chain = DelegationResultSchema.parse(encoded);
    expect(chain.delegations[0].delegation.permissions).toBeUndefined();
  });
});

describe("AuthResponseCodec (legacy authorize-client forwarding)", () => {
  const successResponse = (permissions?: string) => ({
    kind: "authorize-client-success" as const,
    delegations: [
      {
        delegation: new Delegation(PUBKEY, EXPIRATION, undefined, permissions),
        signature: new Uint8Array(64).fill(9),
      },
    ],
    userPublicKey: USER_KEY,
    authnMethod: "passkey" as const,
  });

  it("encodes permissions into the legacy wire form", () => {
    const encoded = AuthResponseCodec.encode(successResponse("queries"));

    if (encoded.kind !== "authorize-client-success") {
      throw new Error("expected success response");
    }
    expect(encoded.delegations[0].delegation.permissions).toBe("queries");
  });

  it("round-trips a queries-only delegation (encode -> parse)", () => {
    const encoded = AuthResponseCodec.encode(successResponse("queries"));
    const decoded = AuthResponseCodec.parse(encoded);

    if (decoded.kind !== "authorize-client-success") {
      throw new Error("expected success response");
    }
    expect(decoded.delegations[0].delegation.permissions).toBe("queries");
  });

  it("round-trips an unrestricted delegation without inventing permissions", () => {
    const encoded = AuthResponseCodec.encode(successResponse(undefined));
    const decoded = AuthResponseCodec.parse(encoded);

    if (decoded.kind !== "authorize-client-success") {
      throw new Error("expected success response");
    }
    expect(decoded.delegations[0].delegation.permissions).toBeUndefined();
  });
});
