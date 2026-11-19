import { describe, expect, it } from "vitest";
import { claimMessage } from "./claimLink";
import { toHex } from "$lib/utils/utils";

describe("claimMessage", () => {
  // The same vector `browser_key.rs` pins for the canister that verifies the claim.
  it("is the domain, the anchor number and the length-prefixed claimer", () => {
    expect(toHex(claimMessage(BigInt(10_000), new Uint8Array([1, 2, 3])))).toBe(
      "69692d73616d652d6465766963652d6c696e6b000000000000271003010203",
    );
  });
});
