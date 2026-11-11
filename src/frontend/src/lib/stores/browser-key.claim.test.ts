import { describe, expect, it } from "vitest";
import { notificationAppClaimMessage } from "./browser-key.store";

describe("notificationAppClaimMessage", () => {
  /** The pair of `the_claim_token_message_is_the_bytes_the_frontend_signs` in `browser_key.rs`, which asserts this same string. */
  it("is the bytes the frontend signs", () => {
    const hex = [
      ...notificationAppClaimMessage(
        BigInt(10_000),
        BigInt(10_001),
        BigInt(5_000),
      ),
    ]
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");

    expect(hex).toBe(
      "69692d6e6f74696669636174696f6e2d6170702d636c61696d000000000000271000000000000027110000000000001388",
    );
  });

  it("covers which identity offers and which claims", () => {
    expect(
      notificationAppClaimMessage(BigInt(1), BigInt(2), BigInt(5_000)),
    ).not.toEqual(
      notificationAppClaimMessage(BigInt(2), BigInt(1), BigInt(5_000)),
    );
  });

  it("covers the expiry, so a holder cannot give it longer to live", () => {
    expect(
      notificationAppClaimMessage(BigInt(1), BigInt(2), BigInt(5_000)),
    ).not.toEqual(
      notificationAppClaimMessage(BigInt(1), BigInt(2), BigInt(6_000)),
    );
  });
});
