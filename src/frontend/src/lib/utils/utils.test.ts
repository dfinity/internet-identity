import { describe, it, expect } from "vitest";
import { transformSignedDelegation } from "$lib/utils/utils";
import type { SignedDelegation } from "$lib/generated/internet_identity_types";

const PUBKEY = new Uint8Array([1, 2, 3, 4]);
const EXPIRATION = BigInt("1700000000000000000");

// Build a candid SignedDelegation as returned by the backend
// (get_account_delegation / get_session_delegation), varying only the
// optional `permissions` field.
const candidSignedDelegation = (
  permissions: [] | [string],
): SignedDelegation => ({
  delegation: {
    pubkey: Array.from(PUBKEY),
    expiration: EXPIRATION,
    targets: [],
    permissions,
  },
  signature: Array.from(new Uint8Array(64)),
});

describe("transformSignedDelegation", () => {
  it("carries a queries-only permissions restriction onto the frontend delegation", () => {
    const result = transformSignedDelegation(
      candidSignedDelegation(["queries"]),
    );

    expect(result.delegation.permissions).toBe("queries");
    // The other fields must still be preserved.
    expect(Array.from(result.delegation.pubkey)).toEqual(Array.from(PUBKEY));
    expect(result.delegation.expiration).toBe(EXPIRATION);
  });

  it("leaves permissions undefined for an unrestricted (full-access) delegation", () => {
    const result = transformSignedDelegation(candidSignedDelegation([]));

    expect(result.delegation.permissions).toBeUndefined();
  });
});
